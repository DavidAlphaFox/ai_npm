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
-define(NPM_TASK_POOL,npm_task_pool).

-record(state, {
	url,
	status,
	headers,
	buffer,
	waiters,
	worker,
	monitors
}).

%%%===================================================================
%%% API
%%%===================================================================
run(Url,Headers)-> npm_cache:run_cache(Url,{fun run_cache/3,[Url,Headers]}).

run_cache(_Url,_Headers,{hit,CacheKey,Headers})-> {hit,CacheKey,Headers};
run_cache(Url,Headers,{expired,Etag,Modified})->
	NewHeaders = npm_http_common:req_headers(Etag,Modified,Headers),
	run_task(Url,[{headers,NewHeaders}]);
run_cache(Url,Headers,not_found)->
	run_task(Url,[{headers,Headers}]).
run_task(Url,Ctx)->
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
		{_Worker,Any} -> 
			npm_task_manager:release(),
			Any
	end;
run_task(Url,_Ctx,Pid)-> 
	npm_package_task:wait(Pid,Url,true).
run(Pid,Url,Ctx)->
	gen_server:call(Pid,{run,Url,Ctx}).
wait(Pid,Url,Before) ->
	Caller = self(),
	case Before of 
		true -> 
			%% 此处会存在另一种竟态，信号量一释放，可能fetcher就被立刻复用了
			%% 因此会出现某个任务在wait之前到达，此时，应当返回一条消息，让等待进程进行重试
			npm_task_manager:release(), 
			gen_server:call(Pid,{wait,Caller,Url},infinity);
		false ->
			try
				gen_server:call(Pid,{wait,Caller,Url},infinity)
			after
				npm_task_manager:release()
			end
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
	erlang:process_flag(trap_exit, true),
	{ok,reset()}.

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
handle_call({run,Url,Ctx},_From,#state{url = undefined} = State)->
	{Result,State1} = do_task(Url,Ctx,State),
	{reply,Result,State1};
handle_call({run,_Url,_Ctx},_From,#state{url = _Any} = State)->
	{reply,{error,not_available},State};
handle_call({wait,Caller,Url},From,#state{url = Url} = State)->
	State1 = add_waiter(Caller,From,State),
	{noreply,State1};
handle_call({wait,_Caller,_Url},_From,State)->
	{reply,{error,gone},State};
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
handle_info({response,fin,Status,Headers},#state{url = Url} = State)->
	Result = cache(Url,Status,Headers),
	State1 = notify_waiters(Result,State),
	{noreply,State1};
handle_info({response,nofin,Status,Headers},State)->
	{noreply,State#state{status = Status,headers = Headers}};
handle_info({data,nofin,Data} ,#state{buffer = Buffer} = State)->
	{noreply,State#state{buffer = <<Buffer/binary,Data/binary>>}};
handle_info({data,fin,Data},#state{url = Url,status = Status,headers = Headers,buffer = Buffer} = State)->
	Result = cache(Url,Status,Headers,<<Buffer/binary,Data/binary>>),
	State1 = notify_waiters(Result,State),
	{noreply,State1};
handle_info({'DOWN', _MRef, process, Pid, _Reason},#state{worker = Worker} = State )->
	State1 = 
		if
			Worker == Pid -> notify_waiters({error,connection_broken},State);
			true -> remove_waiter(Pid,State)
		end,
	{noreply,State1};
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
terminate(_Reason, #state{url = Url}) ->
	if
		Url == undefined -> ok;
		true ->
			Self = self(),
			npm_task_manager:deregister_task(Url,Self)
	end,
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
reset()->
	#state{
		url = undefined,
		status = undefined,
		headers = undefined,
		buffer = <<>>,
		waiters = [],
		worker = undefined,
		monitors = maps:new()
	}.

add_waiter(Caller,From,#state{waiters = W,monitors = M } = State)->
	M1 = ai_process:monitor_process(Caller,M),
	State#state{monitors = M1,waiters = [{Caller,From}|W]}.
remove_waiter(Pid,#state{waiters = W, monitors = M} = State)->
	W1 = lists:filter(fun({Caller,_Frrom}) -> Pid /= Caller end,W),
	M1 = ai_process:demonitor_process(Pid,M),
	State#state{waiters = W1,monitors = M1}.

do_task(Url,Ctx,State)->
	NewCtx = [{url,Url} | Ctx],
	Self = self(),
	case npm_task_manager:register_task(Url,Self) of 
		{run_task,Self}-> do_it(Url,NewCtx,State);
		{run_task,Other}-> {{run_task,Other},State}
	end.
do_it(Url,Ctx,#state{monitors = M} = State)->
	Self = self(),
	RunningFun = fun(Worker)->
		case npm_task_worker:run(Worker,Ctx) of 
			ok -> 
				M2 = ai_process:monitor_process(Worker,M),
				{{run_task,Self},State#state{url = Url,worker = Worker,monitors = M2}};
			{error,not_available} -> 
				npm_task_manager:deregister_task(Url,Self),
				{{error,not_available},State}
		end		
	end,
	poolboy:transaction(?NPM_TASK_POOL, RunningFun).


cache(Url,Status,Headers)->
	if 
		Status == 304 ->
			ai_http_cache:cache(Url,Headers),
			case ai_http_cache:validate_hit(Url) of 
				not_found -> {no_data,Status,Headers};
				{expired,_Etag,_Modified} -> {no_data,Status,Headers};
				Result -> Result
			end;
		true -> {no_data,Status,Headers}
	end.
cache(Url,Status,Headers,Data)->
	if  
		Status  == 200 -> 
			Encoder = ai_http:content_encoding(Headers),
			Data1 = 
				case ai_http:decode_body(Encoder, Data) of 
					{ok,Decoded} -> Decoded;
					_ -> Data
				end,
			do_cache(Url,Headers,Data1);
		true ->  {data,Status,Headers,Data}
	end.
do_cache(Url,Headers,Data)->
	Meta = jsx:decode(Data),
	CacheKey =
		case proplists:get_value(<<"name">>,Meta) of
			undefined -> undefined;
			Name ->
				ScopeName = npm_package_common:scope_name(Name),
				{atomic,ok} = npm_package_mnesia:add(ScopeName,Data),
				ScopeName
		end,
	case CacheKey of 
		undefined -> {data,Headers,Data};
		_ ->
			ai_http_cache:cache(Url,CacheKey,Headers),
			{hit,CacheKey,Headers}
	end.
notify_waiters(Result,#state{url = Url,waiters = W,worker = Worker, monitors = M})->
	Self = self(),
	npm_task_manager:deregister_task(Url,Self),
	M1 = lists:foldl(fun({Caller,From},Acc)->
			NewAcc = ai_process:demonitor_process(Caller,Acc),
			gen_server:reply(From,Result),
			NewAcc
		end,M,W),
	ai_process:demonitor_process(Worker,M1),
	reset().