-module(ai_npm_app).
-behaviour(application).

-include("ai_npm.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    application:ensure_start(crypto),
    application:ensure_start(asn1),
    application:ensure_start(public_key),
    application:ensure_start(ssl),
    application:ensure_start(inets),
    application:ensure_start(syntax_tools),
    application:ensure_start(compiler),
    application:ensure_start(edoc),
    application:ensure_start(ranch),
    application:ensure_start(cowlib),
    application:ensure_start(cowboy),
		application:ensure_start(gun),		
		application:ensure_start(poolboy),
    application:ensure_start(sasl),
    application:ensure_start(ailib),
    application:ensure_start(urilib),
    application:ensure_start(jsx),
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
