-module(npm_task_manager).
-export([start/2]).
-export([lock/1,unlock/1, wait/0,release/0]).
-export([find_task/2,register_task/2,deregister_task/2]).

-spec start(M :: integer(), S :: integer()) -> {module,atom} | {error,badarg}.
start(M,S) 
    when erlang:is_integer(M) and erlang:is_integer(S) 
    and (M > 0) and (S > 0)->
    lists:foreach(fun(I) ->
            Name = name(I),
            {ok,_Mutex} = ai_mutex:new(Name)
        end,lists:seq(0,M-1)),
    {ok,_Semphore} = ai_semaphore:new("npm_task",S),
    npm_task_table = ets:new(npm_task_table,[set,public,named_table,{write_concurrency,true},{read_concurrency,true}]),
    ai_strings:dynamic_module("npm_task_counter.erl",counter_module(M));
start(_M,_N) -> {error,badarg}.

count()-> npm_task_counter:count().

name(I)->  lists:flatten("npm_task_" ++ erlang:integer_to_list(I)).
    
counter_module(N) ->
    lists:flatten(
      ["-module(npm_task_counter).
        -export([count/0]).
        -spec count() -> integer().
        count() ->",
            erlang:integer_to_list(N),
       ".\n"]).

lock(Key)->
    Name = name(erlang:phash2(Key,count())),
    ai_mutex:lock(Name).

unlock(Key)->
    Name = name(erlang:phash2(Key,count())),
    ai_mutex:unlock(Name).

wait()-> ai_semaphore:wait("npm_task").
release()-> ai_semaphore:release("npm_task").

find_task(Url,MFA)->
    ok = lock(Url),
    R = find_task(Url),
    unlock(Url),
    ai_function:run_mfa(MFA,[R]).

find_task(Url)->
    case ets:lookup(npm_task_table,Url) of 
        [{Url,Pid}] -> Pid;
        [] -> not_found
    end.
register_task(Url,Pid)->
    ok = lock(Url),
    R = case find_task(Url) of 
        not_found ->  
            ets:insert(npm_task_table,{Url,Pid}),
            {run_task,Pid};
        OtherPid -> {run_task,OtherPid}
    end,
    unlock(Url),
    R.
deregister_task(Url,Pid)->
    ok = lock(Url),
    R = case find_task(Url) of 
        not_found -> {error,not_found};
        Pid -> 
            ets:delete(npm_task_table,Url),
            {ok,Pid};
        OtherPid -> {error,not_owner,OtherPid}
    end,
    unlock(Url),
    R.
