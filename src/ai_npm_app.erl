-module(ai_npm_app).
-behaviour(application).

-include_lib("ailib/include/ailib.hrl").
-include("npm_package.hrl").
-include("npm_tarball.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    application:ensure_started(crypto),
    application:ensure_started(asn1),
    application:ensure_started(public_key),
    application:ensure_started(ssl),
    application:ensure_started(inets),
    application:ensure_started(syntax_tools),
    application:ensure_started(compiler),
    application:ensure_started(edoc),
    application:ensure_started(ranch),
    application:ensure_started(cowlib),
    application:ensure_started(cowboy),
	application:ensure_started(gun),		
	application:ensure_started(poolboy),
    application:ensure_started(sasl),
    application:ensure_started(ailib),
    application:ensure_started(urilib),
    application:ensure_started(jsx),
    ai_npm_storage:ensure_default_storage(),
    Result = ai_npm_sup:start_link(),
    ai_mnesia:ensure(fun()-> create_db() end),
    ai_idempotence_pool:named_pool(pkg),
    Port = application:get_env(ai_npm,api_server_port,4873),
    {ok,_} = npm_api_server:start(Port),
    Result.

stop(_State) ->
    ai_npm_proxy_server:stop(),
    ok.
create_db()->
    {atomic,ok} = mnesia:create_table(package,[{disc_copies, [node()]},  
                                               {attributes, record_info(fields, package)}]),
    {atomic,ok} = mnesia:create_table(tarball,[{disc_copies, [node()]}, {type,bag},
                                               {attributes, record_info(fields, tarball)}]),
    ai_http_cache:initialize_mnesia_table(),
    ok.
