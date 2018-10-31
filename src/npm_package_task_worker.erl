%%%-------------------------------------------------------------------
%%% @author David Gao <david@laptop-02.local>
%%% @copyright (C) 2018, David Gao
%%% @doc
%%% idempotence task can use one request to answer all at same time
%%% @end
%%% Created : 15 Oct 2018 by David Gao <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(npm_package_task_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3, format_status/2]).
-export([task_run/3]).
-define(SERVER, ?MODULE).
-define(RESCHEDULE_TIMEOUT,1000).
-define(HTTP_TIMEOUT,10000).
-record(state, {
				conn,
				mref,
				stream,
				timer,
				current,
				url,
				status,
				headers,
				cache_key,
				buffer,
				queue
				}).

%%%===================================================================
%%% API
%%%===================================================================
task_run(Worker,Key,Ctx)->
    Caller = self(),
    gen_server:cast(Worker,{task,Key,Ctx,Caller}).
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
	gen_server:start_link(?MODULE,Args, []).

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
%%	process_flag(trap_exit, true),
	{ok, #state{conn = undefined,mref = undefined, 
				stream = undefined, current = undefined,
				status = undefined,headers = undefined,
				buffer = <<>>,queue = queue:new()}}.

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
handle_cast({task,Key,Ctx,Caller},#state{current = undefined} = State)->
	State1 = State#state{current = {Key,Ctx,Caller}},
	State2 = do_task(Ctx,State1),
	{noreply,State2};
handle_cast({task,Key,Ctx,Caller}, #state{queue = Q} = State)->
	{noreply,State#state{queue = queue:in({Key,Ctx,Caller},Q)}};
handle_cast({hit,_CacheKey,_Headers} = M,#state{current = Task} = State)->
	io:format("hit task~p~n",[Task]),
	reply(Task,M),
	State1 = clean(State),
	State2 = schedule_task(State1),
	{noreply,State2};
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
handle_info({kill_tasks,_StreamRef}, #state{mref = MRef,current = Task} = State)->
	reply(Task,{error,timeout}),
	State1 = kill_timer(State),
	State2 = down(MRef,undefined,State1),
	State3 = schedule_task(clean(State2)),
	{noreply,State3};
handle_info({reschedule_tasks,Ctx}, State)->
	State1 = do_task(Ctx,State),
	{noreply,State1};
handle_info({gun_response, ConnPid, StreamRef, fin, Status, Headers},
	#state{conn = ConnPid,stream = StreamRef,current = Task,url = Url,cache_key = CacheKey} = State) -> 
	State1 = kill_timer(State),
	Result = process_task(Url,Status,Headers,CacheKey,no_data),
	reply(Task,Result),
	State2 = schedule_task(clean(State1)),
	{noreply,State2};
handle_info({gun_response, ConnPid, StreamRef, nofin, Status, Headers},
	#state{conn = ConnPid,stream = StreamRef} = State) -> 
	State1 = kill_timer(State),
	{noreply,State1#state{ status = Status,headers = Headers}};

handle_info({gun_data, ConnPid, StreamRef, nofin, Data},
	#state{conn = ConnPid,stream = StreamRef,buffer = Buffer} = State)->
		{noreply,State#state{buffer = <<Buffer/binary,Data/binary>>}};
handle_info({gun_data, ConnPid, StreamRef, fin, Data},
	#state{conn = ConnPid,stream = StreamRef,current = Task, 
		url = Url,status = Status,headers = Headers,
		cache_key = CacheKey,buffer = Buffer} = State)->
	Final = <<Buffer/binary,Data/binary>>,
	Result = process_task(Url,Status,Headers,CacheKey,Final),
	reply(Task,Result),
	State1 = schedule_task(clean(State)),
	{noreply,State1};
handle_info({'DOWN', MRef, process, ConnPid, _Reason},#state{conn = ConnPid,mref = MRef,current = Task} = State )->
	State1 = down(MRef,Task,State),
	{noreply,State1};
handle_info({gun_down, ConnPid, _Protocol,
             _Reason, _Killed, _Unprocessed},
            	#state{conn = ConnPid,mref = MRef,current = Task} = State) ->
	State1 = down(MRef,Task,State),
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
timer(Timeout,TimeoutMsg,#state{timer = PrevRef} = State)->
    if PrevRef == undefined -> ok;
       true -> erlang:cancel_timer(PrevRef)
    end,
    Ref = erlang:send_after(Timeout, self(), TimeoutMsg),
    State#state{timer = Ref}.
kill_timer(#state{timer = PrevRef} =State)->
	if PrevRef == undefined -> ok;
       true -> erlang:cancel_timer(PrevRef)
    end,
	State#state{timer = undefined}.
demonitor_process(undefined) -> true;
demonitor_process(MRef) -> erlang:demonitor(MRef).

conn(Ctx,#state{conn = undefined} = State)->
	{ok, ConnPid} = npm_fetcher:open(Ctx),
	MRef = erlang:monitor(process, ConnPid),
	case gun:await_up(ConnPid) of 
		{error, _Reason} ->
			demonitor_process(MRef),
			{undefined,State};
		{ok, _Protocol} ->
			{ConnPid, State#state{conn = ConnPid,mref = MRef}}
	end;
conn(_Ctx,#state{conn = Conn} = State)->
	{Conn,State}.

clean(State)->
	State#state{
			stream = undefined, current = undefined,
			status = undefined,headers = undefined,
			buffer = <<>>}.

down(MRef,Task,State)->
	State1 = kill_timer(State),
  	demonitor_process(MRef),
	State2 = clean(State1),
	State3 = State2#state{current = Task,conn = undefined,mref = undefined},
	case Task of 
		undefined -> State3;
		{_Key,Ctx,_Caller} -> timer(?RESCHEDULE_TIMEOUT,{reschedule_tasks,Ctx},State3)
	end.
reply(undefined,_Result)-> ok;
reply(Task,Result)->
	{Key,_Ctx,Caller} = Task,
	ai_idempotence_pool:task_finish(Caller,Key,{done,Result}).
schedule_task(#state{queue = Q} = State) ->
 	case queue:out(Q) of
        {{value,NextTask},NQ} ->
			{_Key,Ctx,_Caller} = NextTask,
			State1 = State#state{current = NextTask,queue = NQ},
			do_task(Ctx,State1);
		_ ->
			State
	end.


do_task(Ctx,State)->
    Url = proplists:get_value(url,Ctx),
	CacheKey = proplists:get_value(cache_key,Ctx),
    Fun = fun(CacheHint) ->
        do_on_cache(CacheHint,Ctx,State#state{cache_key = CacheKey})
    end,
    npm_cache:run_cache(Url,{Fun,[]}).

do_on_cache({hit,CacheKey,Headers},_Ctx,State)-> 
	gen_server:cast(self(),{hit,CacheKey,Headers}),
	State;
do_on_cache(_,Ctx,State)->
    Url = proplists:get_value(url, Ctx),
    ReqHeaders = npm_fetcher:headers(Ctx),
	{ConnPid,State1} = conn(Ctx,State),
	case ConnPid of 
		undefined -> 
			timer(?RESCHEDULE_TIMEOUT,{reschedule_tasks,Ctx},State1);
		_ ->
    		StreamRef = gun:get(ConnPid, Url, ReqHeaders),
			timer(?HTTP_TIMEOUT,{kill_tasks,StreamRef},State1#state{stream = StreamRef,url = Url})
	end.
process_task(Url,Status,Headers,CacheKey,no_data)->cache(no_data,Url,Status,Headers,CacheKey);
process_task(Url,Status,Headers,CacheKey,Final)->
	Encoder = proplists:get_value(<<"content-encoding">>,Headers),
	Body = npm_fetcher:decode_body(Encoder, Final),
	if  
    	Status  == 200 ->  cache(data,Url,Headers,Body,CacheKey);
        true ->  {data,Status,Headers,Body}
    end.

cache(data,Url,Headers,Body,_CacheKey)->
    Meta = jsx:decode(Body),
    CacheKey =
        case proplists:get_value(<<"name">>,Meta) of
            undefined -> undefined;
            Name ->
                ScopeName = npm_package:scope_name(Name),
                {atomic,ok} = npm_package_mnesia:add(ScopeName,Body),
                ScopeName
        end,
    case CacheKey of 
        undefined ->
            {data,Headers,Body};
        _ ->
            ai_http_cache:cache(Url,CacheKey,Headers),
            {hit,CacheKey,Headers}
    end;
cache(no_data,Url,Headers,Status,CacheKey)->
    if 
        Status == 304 ->
            ai_http_cache:cache(Url,Headers),
            {hit,CacheKey,Headers};
        true -> {no_data,Status,Headers}
    end.