%%%-------------------------------------------------------------------
%%% @author David Gao <david@laptop-02.local>
%%% @copyright (C) 2018, David Gao
%%% @doc
%%% idempotence task can use one request to answer all at same time
%%% @end
%%% Created : 15 Oct 2018 by David Gao <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(npm_tarball_task_worker).

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
				fd,
				queue,
				stage
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
				fd = undefined,tmpfile = undefined,queue = queue:new()}}.

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
handle_info({gun_response,ConnPid,StreamRef,fin,Status,Headers},
	#state{conn = ConnPid,stream = StreamRef,stage = head} = State)->
	Ctx = current_task_ctx(State),
	State1 = kill_timer(State),
	State2 = process_head(Status,Headers,Ctx,State1),
	{noreply,State2};
handle_info({gun_response, ConnPid, StreamRef, fin, Status, Headers},
	#state{conn = ConnPid,stream = StreamRef,} = State) -> 
	Ctx = current_task_ctx(State)
	State1 = kill_timer(State),
	Result = process_task(Ctx,Status,Headers,no_data),
	reply(Task,Result),
	State2 = schedule_task(clean(State1)),
	{noreply,State2};
handle_info({gun_response, ConnPid, StreamRef, nofin, Status, Headers},
	#state{conn = ConnPid,stream = StreamRef,current = Task, tarball = Tar} = State) -> 
	State1 = kill_timer(State),
    {Scope,_Package,_Version,Tarball} = Tar,
    Tmpfile = npm_tarball_storage:tmpfile(Scope,Tarball),
    case ai_blob_file:open_for_write(Tmpfile) of
        {ok,Fd} ->
            {noreply,State1#state{ status = Status,headers = Headers,fd = Fd,tmpfile = Tmpfile}};
        Error ->
            reply(Task,Error),
            State2 = schedule_task(clean(State1)),
	        {noreply,State2}
    end;
handle_info({gun_data, ConnPid, StreamRef, nofin, Data},
	#state{conn = ConnPid,stream = StreamRef,current = Task,fd = OFile} = State)->
        case ai_blob_file:write(OFile, Data) of 
            {ok,NewFd,_Size} -> {noreply,State#state{fd = NewFd}};
            Error ->
                reply(Task,Error),
                State1 = schedule_task(clean(State)),
	            {noreply,State1}
        end;
handle_info({gun_data, ConnPid, StreamRef, fin, Data},
	#state{conn = ConnPid,stream = StreamRef,current = Task, 
		url = Url,status = Status,headers = Headers,
		tarball = Tar,fd = OFile,tmpfile = Tmpfile} = State)->
    {ok,NewFd,_Size} = ai_blob_file:write(OFile, Data),
	{ok,_NewFd2,Digest} = ai_blob_file:close(NewFd),
	Result = process_task(Url,Status,Headers,Tar,{Tmpfile,Digest}),
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


current_task_ctx(State)->
	{_Key,Ctx,_Caller} = State#state.current,
	Ctx.


clean(State)->
    case State#state.fd of 
        undefined -> ok;
        Fd -> ai_blob_file:close(Fd)
    end,
	State#state{
			stream = undefined, current = undefined,
			status = undefined,headers = undefined,
			tarball = undefined, fd = undefined,tmpfile = undefined}.

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
			run_cache(Ctx,State1);
		_ ->
			State
	end.

tarball(Ctx)->
    Scope = proplists:get_value(scope,Ctx),
    Version = proplists:get_value(version,Ctx),
    Package = proplists:get_value(package,Ctx),
    Tarball = proplists:get_value(tarball,Ctx),
    {Scope,Package,Version,Tarball}.



process_task(Url,Status,Headers,Tar,no_data)->cache(no_data,Url,Headers,Status,Tar);
process_task(Url,_Status,Headers,Tar,{Tmpfile,Digest})->
	DigestString = ai_strings:hash_to_string(Digest,160,lower),
	{Scope,Package,Version,Tarball} = Tar,
    case npm_tarball_storage:store(Tmpfile,DigestString,Scope,Tarball) of
        {ok, FinalFile} -> 
            cache(data,Url,Headers,FinalFile,Tar),
            {hit,{Scope,Package,Version},Headers};
		Error -> Error
	end.

cache(no_data,Url,Headers,Status,Tar)->
    {Scope,Package,Version,_Tarball} = Tar,
    if 
        Status == 304 ->
            ai_http_cache:cache(Url,Headers),
            {hit,{Scope,Package,Version},Headers};
        true ->
            {no_data,Status,Headers}
    end;
cache(data,Url,Headers,Path,Tar)->
    {Scope,Package,Version,_Tarball} = Tar,
    npm_tarball_mnesia:add({Scope,Package},Version,Path),
    ai_http_cache:cache(Url,{Scope,Package,Version},Headers).

do_head(Ctx,State)->
	Url = proplists:get_value(url,Ctx),
	ReqHeaders = npm_fetcher:headers(Ctx),
	{ConnPid,State1} = conn(Ctx,State),
	case ConnPid of 
		undefined -> 
			timer(?RESCHEDULE_TIMEOUT,{reschedule_tasks,Ctx},State1);
		_ ->
    		StreamRef = gun:head(ConnPid, Url, ReqHeaders),
			timer(?HTTP_TIMEOUT,{kill_tasks,StreamRef},
				State1#state{stream = StreamRef,url = Url,stage = head})
	end.
process_head(Status,Headers,Ctx,State)->
	if 
		Status == 200 -> do_get(ai_http:accept_ranges(Headers),Headers,Ctx,State);
		true -> do_get(false,Headers,Ctx,State)
	end.

do_get(false,_Headers,Ctx,State)->
	do_task(Ctx,State#state{stage = reset});
do_get(true,Headers,Ctx,State)->
	LastModified = ai_http:last_modified(Headers),
	Etag = ai_http:etag(Headers),
	Length = ai_http:content_length(Headers),
	case npm_tarball_file:resume(Fd,{Etag,LastModified,Length}) of 
		{ok,Received} -> do_task(Ctx,State#state{stage = {resume,Received}});
		Error -> timer(?RESCHEDULE_TIMEOUT,{reschedule_tasks,Ctx},State1)
	end.
do_task(Ctx,#state{stage = Stage} =  State)->
	case Stage of
		reset -> 
			Headers = npm_fetcher:headers(Ctx),
			do_task(Ctx,Headers,State#state{stage = reset}).
		{resume,Received} ->
			BinInteger = erlang:integer_to_binary(Received),
			Headers = npm_fetcher:headers(Ctx) ++ [{<<"range">>,<<"bytes=",BinInteger/binary,"-">>}],
			do_task(Ctx,Headers,State#state{stage = resume})
	end.
	
do_task(Ctx,Headers,State)->
	Url = proplists:get_value(url,Ctx),
	case ConnPid of 
		undefined -> 
			timer(?RESCHEDULE_TIMEOUT,{reschedule_tasks,Ctx},State1);
		_ ->
    		StreamRef = gun:head(ConnPid, Url, Headers),
			timer(?HTTP_TIMEOUT,{kill_tasks,StreamRef},
				State1#state{stream = StreamRef})
	end.
run_cache(Ctx,State)->
    Url = proplists:get_value(url,Ctx),
    Fun = fun(CacheHint) ->
        run_cache(CacheHint,Ctx,State)
    end,
    npm_cache:run_cache(Url,{Fun,[]}).
run_cache({hit,CacheKey,Headers},_Ctx,State)-> 
	gen_server:cast(self(),{hit,CacheKey,Headers}),
	State;
run_cache(_,Ctx,State)->
	Url = proplists:get_value(url,Ctx),
	case npm_tarball_manager:file(Url) of 
		undefined -> do_head(Ctx,State)
		Pid -> 
			gen_server:cast(self(),{pid,Pid}),
			State
	end.
			