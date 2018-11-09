-module(npm_task_locker).
-export([start/1]).
-export([lock/1,release/1]).

-spec start(N :: integer()) -> {module,atom} | {error,badarg}.
start(N) when erlang:is_integer(N) and N > 0->
    lists:foreach(fun(I) ->
            Name = name(I),
            {ok,_Mutex} = ai_mutex:start(Name), 
            {ok,_Semphore} = ai_mutex:start(Name,N)
        end,lists:seq(0,N-1)),
    ai_strings:dynamic_module("npm_task_locker_count.erl",locker_state(N));
start(_N) -> {error,badarg}.

count()-> npm_task_locker_count:count().

name(I)-> lists:flatten(["task_",erlang:integer_to_list(I)]).
    
locker_state(N) ->
    lists:flatten(
      ["-module(npm_task_locker_count).
        -export([count/0]).
        -spec count() -> integer().
        count() ->",
        erlang:integer_to_list(N),
       ".\n"]).

lock(Key)->
    Name = name(erlang:phash2(Key,count())),
    case ai_semaphore:wait(Name) of 
        ok -> 
            case ai_mutex:lock(Name) of 
                ok -> ok;
                destroy-> 
                    ai_semaphore:release(Name),
                    exit({error,destroy})
            end;
        destroy ->
            exit({error,destroy})
    end.
release(Key)->
    Name =  name(erlang:phash2(Key,count())),
    ai_mutex:release(Name),
    ai_semaphore:release(Name).