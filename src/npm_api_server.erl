-module(npm_api_server).
%% API
-export([start/1, stop/0]).

-spec start(inet:port_number()) -> {ok, pid()}.
start(Port) ->
    Router =  {'_', [
%%                     {"/-/v1/login",ai_npm_user_handler,[]},
                     {"/-/user/:user",npm_api_user,[]},
                     {"/[:scope]/:package/-/:tarball", npm_api_tarball,[]},
                     {"/:scope/[:package/[:version]]",npm_api_package,[]}
                    ]},
    Dispatch = cowboy_router:compile([Router]),
    io:format("Dispatch ~p~n",[Dispatch]),
    cowboy:start_clear(ai_npm_proxy_server,
                       [{port, Port}],
                       #{env => #{dispatch => Dispatch}}
                      ).


stop() ->
    cowboy:stop_listener(ai_npm_proxy_server).
