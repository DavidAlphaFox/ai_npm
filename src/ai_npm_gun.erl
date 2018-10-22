-module(ai_npm_gun).

-export([decode_body/2]).
-export([remote_host/1,remote_protocol/1,remote_port/1]).
-export([tls/1,headers/2]).
-export([open/1]).

decode_body(<<"gzip">>,Body)->zlib:gunzip(Body);
decode_body(_,Body) -> Body.

open(Ctx)->
    Host = remote_host(Ctx),
    Protocol = remote_protocol(Ctx),
    Port = remote_port(Ctx),
    gun:open(Host, Port,tls(Protocol)).

remote_host(Ctx)-> proplists:get_value(uplink,Ctx,"registry.npmjs.org").
remote_port(Ctx) -> proplists:get_value(uplink_port,Ctx,443).
remote_protocol(Ctx) -> proplists:get_value(uplink_protocol,Ctx,"https").
tls("https")->#{transport => tls};
tls(_) ->#{}.    

headers(Headers,Ctx)->
    NewHeaders = lists:filter(fun({Key,_Value}) -> Key /= <<"host">> end,Headers),
    Host = remote_host(Ctx),
    Port = remote_port(Ctx),
    HostHeader = 
        if 
            (Port == 80) or (Port == 443) ->
                {<<"host">>,erlang:list_to_binary(Host)};
            true ->
                {<<"host">>,erlang:list_to_binary([Host,":",erlang:integer_to_list(Port)])}
        end,
    [HostHeader|NewHeaders].
