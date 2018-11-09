%%%-------------------------------------------------------------------
%%% @author  <david@laptop-02.local>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2018 by  <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(npm_package_task).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3, format_status/2]).

-export([run/2,run/3,wait/3]).

-define(SERVER, ?MODULE).
-define(PACKAGE_TASK_POOL,package_task_pool).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
run(Url,Ctx)->
    npm_cache:run_cache(Url,{fun run_cache/3,[Url,Ctx]}).
run_cache(Url,_Ctx,{hit,CacheKey,Headers})-> 
	npm_task_manager:done(Url),
	{hit,CacheKey,Headers};
run_cache(Url,Ctx,_)->
	%% Cache 没有找到，需要到Task队列中找一下
	%% 会自动加锁解锁，函数在锁范围之外运行
	npm_task_manager:find_task(Url,{fun run_task/3,[Url,Ctx]}).

run_task(Url,Ctx,not_found)->
	ok = npm_task_manager:wait(), %% 拿信号量
	RunningFun = fun(Worker)->
		Result = npm_package_task:run(Worker,Url,Ctx),
		{Worker,Result}
	end,
	%% 先释放了锁，这里面就存在了一个竞争态的问题
	%% 如果run_task返回的Pid和Worker是同一个，说明我们用了一个fetcher
	%% 就不能释放信号量。但是如果不同则需要释放信号量
	case poolboy:transaction(?PACKAGE_TASK_POOL, RunningFun) of 
		{Pid,{run_task,Pid}} -> npm_package_task:wait(Pid,Url,false);  
		{_Worker,{run_task,Pid}}-> npm_package_task:wait(Pid,Url,true);
		Any -> 
			npm_task_manager:release(),
			Any
	end;
run_task(Url,_Ctx,Pid)-> 
	npm_package_task:wait(Pid,Url,true).
run(Pid,Url,Ctx)->
	Caller = self(),
	gen_server:call(Pid,{run,Caller,Url,Ctx}).
wait(Pid,Url,Before) ->
	Caller = self(),
	case Before of 
		true -> 
			%% 此处会存在另一种竟态，信号量一释放，可能fetcher就被立刻复用了
			%% 因此会出现某个任务在wait之前到达，此时，应当返回一条消息，让等待进程进行重试
			npm_task_manager:release(), 
			gen_server:call(Pid,{wait,Caller,Url});
		false ->
			R = gen_server:call(Pid,{wait,Caller,Url}),
			npm_task_manager:release(),
			R
	end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: proplists:proplists()) -> {ok, Pid :: pid()} |
	{error, Error :: {already_started, pid()}} |
	{error, Error :: term()} |
	ignore.
start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	{ok, State :: term(), Timeout :: timeout()} |
	{ok, State :: term(), hibernate} |
	{stop, Reason :: term()} |
	ignore.
init(_Args) ->
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	{reply, Reply :: term(), NewState :: term()} |
	{reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	{reply, Reply :: term(), NewState :: term(), hibernate} |
	{noreply, NewState :: term()} |
	{noreply, NewState :: term(), Timeout :: timeout()} |
	{noreply, NewState :: term(), hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	{stop, Reason :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	{noreply, NewState :: term()} |
	{noreply, NewState :: term(), Timeout :: timeout()} |
	{noreply, NewState :: term(), hibernate} |
	{stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	{noreply, NewState :: term()} |
	{noreply, NewState :: term(), Timeout :: timeout()} |
	{noreply, NewState :: term(), hibernate} |
	{stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
	State :: term()) -> any().
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
	State :: term(),
	Extra :: term()) -> {ok, NewState :: term()} |
	{error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
	Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
	Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
