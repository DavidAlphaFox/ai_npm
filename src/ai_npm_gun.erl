-module(ai_npm_gun).
-export([fetch_without_cache/1]).

fetch_without_cache(Ctx)->
    {ok,ConnPid} = open_remote(Ctx),
    {ok, _Protocol} = gun:await_up(ConnPid),
    Url = proplists:get_value(url,Ctx),
    StreamRef = gun:get(ConnPid, Url),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, Status, Headers} ->
            {no_data,Status,Headers};
        {response, nofin, Status, Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            {data,Status,Headers,Body}
    end.

open_remote(Ctx)->
    Remote = proplists:get_value(uplink,Ctx,"registry.npmjs.org"),
    Protocol = proplists:get_value(uplink_protocol,Ctx,"https"),
    Port = proplists:get_value(uplink_port,Ctx,443),
    gun:open(Remote, Port,tls(Protocol)).

tls("https")->#{transport => tls};
tls(_) ->#{}.
