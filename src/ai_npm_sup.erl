-module(ai_npm_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    npm_task_manager:start(40,10),
    PoolSpecs = worker_pool_specs(),
    {ok, {SupFlags, PoolSpecs}}.

default_woker_pool()->
      [
       {package_task_pool,
        [{size,5},{max_overflow,0},{worker_module,npm_package_task},{strategy,fifo}],[]},
       {npm_task_pool,
        [{size,10},{max_overflow,0},{worker_module,npm_task_worker},{strategy,fifo}],[]}
      ].
worker_pool_specs()->
    lists:map(fun({Name, Args, WorkerArgs}) ->
                      PoolArgs = [{name, {local, Name}}] ++ Args,
                      poolboy:child_spec(Name, PoolArgs, WorkerArgs)
              end, default_woker_pool()).
