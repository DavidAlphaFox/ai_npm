-module(ai_npm_proxy_server).
%% API
-export([start/1, stop/0]).

-spec start(inet:port_number()) -> {ok, pid()}.
start(Port) ->
    Router =  {'_', [
                     {"/:package/[:version]",ai_npm_package_handler,[]},
                     {"/:package/-/:tarball", ai_npm_tarball_handler,[]}
                    ]},
    Dispatch = cowboy_router:compile([Router]),
    cowboy:start_clear(ai_npm_proxy_server,
                       [{port, Port}],
                       #{env => #{dispatch => Dispatch}}
                      ).


stop() ->
    cowboy:stop_listener(ai_npm_proxy_server).
