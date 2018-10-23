-module(ai_npm_app).
-behaviour(application).

-include("ai_npm.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    ai_npm_storage:ensure_default_storage(),
    Result = ai_npm_sup:start_link(),
    ai_mnesia:ensure(fun()-> create_db() end),
    ai_idempotence_pool:named_pool(pkg),
    Port = application:get_env(ai_npm,proxy_server_port,4873),
    {ok,_} = ai_npm_proxy_server:start(Port),
    Result.

stop(_State) ->
    ai_npm_proxy_server:stop(),
    ok.
create_db()->
    {atomic,ok} = mnesia:create_table(package,[{disc_copies, [node()]},  
                                               {attributes, record_info(fields, package)}]),
    {atomic,ok} = mnesia:create_table(cache,  [{disc_copies, [node()]},  
                                                       {attributes, record_info(fields, cache)}]),

    ok.
