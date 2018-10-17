-module(ai_npm_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Result = ai_npm_sup:start_link(),
    ai_idempotence_pool:named_pool(pkg),
    Port = application:get_env(ai_npm,proxy_server_port,4873),
    {ok,_} = ai_npm_proxy_server:start(Port),
    Result.

stop(_State) ->
    ai_npm_proxy_server:stop(),
    ok.
