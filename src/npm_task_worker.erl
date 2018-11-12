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
-export([run/2]).
-define(SERVER, ?MODULE).
-define(RESCHEDULE_TIMEOUT,1000).
-define(HTTP_TIMEOUT,10000).
-record(state, {
                conn,
                stream,
                uplink,
                timer,
                task,
                receiver,
                monitors 
				}).

%%%===================================================================
%%% API
%%%===================================================================
run(Pid,Ctx)->
    Caller = self(),
    gen_server:call(Pid,{run, Caller,Ctx}).

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
init(Args) ->
%%	process_flag(trap_exit, true),
    {ok, #state{conn = undefined,stream = undefined,uplink = Args,task = undefined,
                timer = ai_timer:new(), monitors = maps:new()}}.

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
handle_call({run,Caller,Ctx},_From,#state{task = undefiend} = State)->
    State1 = do_task(Caller,Ctx,State),
    {reply,ok,State1};
handle_call({run,_Caller,_Ctx},_From,#state{task = _Any} =  State)->
    {reply,{error,not_available},State};
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
handle_info({timeout,StreamRef},#state{conn = ConnPid, stream = StreamRef, receiver = Receiver, timer = Timer,monitors = M } = State)->
    Timer1 = ai_timer:cancel(Timer),
    M1 = ai_process:demonitor_process(ConnPid,M),
    M3 =  case Receiver of 
                undefiend -> M1;
                _ ->  
                    M2 = ai_process:demonitor_process(Receiver,M1),
                    receiver ! {error,timeout},
                    M2
            end,
    gun:close(ConnPid),
    {noreply,State#state{
        conn = undefiend,stream = undefiend,
        receiver = undefiend,task = undefiend,
        timer = Timer1,monitors = M3
    }}; 
handle_info({timeout,_StreamRef},State)->
    {noreply,State};
handle_info({gun_response, ConnPid, StreamRef, fin, Status, Headers},
	#state{conn = ConnPid,stream = StreamRef,receiver  = Receiver,timer = Timer,monitors = M} = State) -> 
    Timer1 = ai_timer:cancel(Timer),
    Receiver ! {response,fin,Status,Headers},
    M1 = ai_process:demonitor_process(Receiver,M),
    {noreply,State#state{stream = undefiend,receiver = undefiend, task = undefiend, timer = Timer1,monitors = M1}};

handle_info({gun_response, ConnPid, StreamRef, nofin, Status, Headers},
	#state{conn = ConnPid,stream = StreamRef,receiver  = Receiver,timer = Timer} = State) -> 
    Timer1 = ai_timer:restart(Timer),
    Receiver ! {response,nofin,Status,Headers},
	{noreply,State#state{timer = Timer1}};

handle_info({gun_data, ConnPid, StreamRef, nofin, Data},
	#state{conn = ConnPid,stream = StreamRef,receiver  = Receiver,timer = Timer} = State)->
    Timer1 = ai_timer:restart(Timer),
    Receiver ! {data,nofin,Data},
	{noreply,State#state{timer = Timer1}};
handle_info({gun_data, ConnPid, StreamRef, fin, Data},
	#state{conn = ConnPid,stream = StreamRef,receiver  = Receiver,timer = Timer,monitors = M} = State)->
    Timer1 = ai_timer:cancel(Timer),
    Receiver ! {data,fin,Data},
    M1 = ai_process:demonitor_process(Receiver,M),
    {noreply,State#state{stream = undefiend, receiver = undefiend, task = undefiend, timer = Timer1,monitors = M1}};
    
handle_info({'DOWN', _MRef, process, Pid, _Reason},#state{conn = ConnPid,receiver = Receiver} = State )->
    State1 = if
        Pid == ConnPid -> gun_down(State);  
        Pid == Receiver ->  receiver_down(State);
        true -> State     
    end,
	{noreply,State1};
handle_info({gun_down, ConnPid, _Protocol,
             _Reason, _Killed, _Unprocessed},
            	#state{conn = ConnPid} = State) ->
	State1 = gun_down(State),
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

gun_down(#state{conn = ConnPid, receiver = undefined,monitors = M} =  State)->
    M1 = ai_process:demonitor_process(ConnPid,M),
    State#state{conn = undefined,stream = undefined,monitors = M1};
gun_down(#state{conn = ConnPid,receiver = Receiver, timer = Timer, monitors = M} = State)->
    Timer1  = ai_timer:cancel(Timer),
    M1 = ai_process:demonitor_process(ConnPid,M),
    M2 = ai_process:demonitor_process(Receiver,M1),
    Receiver ! {error,broken},
    State#state{conn = undefined,stream = undefined, receiver = undefined,task = undefined,timer = Timer1,monitors = M2}.
receiver_down(#state{conn = ConnPid, receiver  = Receiver,timer = Timer, monitors = M} = State)->
    Timer1 = ai_timer:cancel(Timer),
    M1 = ai_process:demonitor_process(ConnPid,M),
    M2 = ai_process:demonitor_process(Receiver,M1),
    gun:close(ConnPid),
    State#state{
        conn = undefiend,stream = undefiend,
        receiver = undefiend,task = undefiend,
        timer = Timer1,monitors = M2
    }.

open(#state{conn = undefiend,uplink = Ctx,monitors = M} = State)->
    {ok,ConnPid} = npm_fetcher:open(Ctx),
    case gun:await_up(ConnPid) of 
        {error,_Reason} -> {undefiend,State};
        {ok,_Protocol} ->
            M1 = ai_process:monitor_process(ConnPid,M),
            {ConnPid,State#state{conn = ConnPid,monitors = M1}}
    end;
open(#state{conn = ConnPid} = State)-> {ConnPid,State}.
do_task(Caller,Ctx,State)->
    Url = proplists:get_value(url, Ctx),
    Headers = npm_fetcher:headers(Ctx),
    {ConnPid,State1} = open(State),
    case ConnPid of 
        undefiend -> 
            Caller ! {error,broken},
            State1;
        _ ->
            StreamRef = gun:get(ConnPid,Url,Headers),
            Timer = State1#state.timer,
            Timer1 = ai_timer:start(?HTTP_TIMEOUT,{timeout,StreamRef},Timer),
            State#state{timer = Timer1,receiver = Caller,task = Ctx}
    end.
