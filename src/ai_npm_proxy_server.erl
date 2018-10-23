-module(ai_npm_proxy_server).
%% API
-export([start/1, stop/0]).

-spec start(inet:port_number()) -> {ok, pid()}.
start(Port) ->
    Router =  {'_', [
                     {"/:package/[:version]",ai_npm_package_handler,[]},
%%                     {"/-/v1/login",ai_npm_user_handler,[]},
                     {"/-/user/:user",ai_npm_user_handler,[]},
                     {"/:package/-/:tarball", ai_npm_tarball_handler,[]}
                    ]},
    Dispatch = cowboy_router:compile([Router]),
    io:format("Dispatch ~p~n",[Dispatch]),
    cowboy:start_clear(ai_npm_proxy_server,
                       [{port, Port}],
                       #{env => #{dispatch => Dispatch}}
                      ).


stop() ->
    cowboy:stop_listener(ai_npm_proxy_server).
