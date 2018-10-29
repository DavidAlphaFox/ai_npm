-module(npm_api_user).
-export([init/2]).
init(Req,State)->
    io:format("got req ~p~n",[Req]),
    {ok, Data, Req0} = cowboy_req:read_body(Req),
    io:format("got data ~p~n",[Data]),
    {ok,Req0,State}.
