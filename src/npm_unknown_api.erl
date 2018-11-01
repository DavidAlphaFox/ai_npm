-module(npm_unknown_api).
-export([init/2]).
init(Req,State)->
    Url = cowboy_req:path(Req),
    io:format("unknown_api:  ~p~n ~p~n",[Url,Req]),
    {ok,Req,State}.
