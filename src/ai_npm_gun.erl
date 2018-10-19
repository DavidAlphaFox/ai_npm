-module(ai_npm_gun).
-export([fetch_without_cache/1]).

fetch_without_cache(Ctx)->
    {ok,ConnPid} = open_remote(Ctx),
    {ok, _Protocol} = gun:await_up(ConnPid),
    Url = proplists:get_value(url,Ctx),
    ReqHeaders =  uplink_headers(proplists:get_value(headers,Ctx,[]),Ctx),
    CacheProcessor = proplists:get_value(processor,Ctx),
    StreamRef = gun:get(ConnPid, Url,ReqHeaders),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, Status, Headers} ->
            CacheProcessor(Status,Headers,no_data);
        {response, nofin, Status,Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            CacheProcessor(Status,Headers,Body)
    end.



open_remote(Ctx)->
    Host = remote_host(Ctx),
    Protocol = remote_protocol(Ctx),
    Port = remote_port(Ctx),
    gun:open(Host, Port,tls(Protocol)).

remote_host(Ctx)-> proplists:get_value(uplink,Ctx,"registry.npmjs.org").
remote_port(Ctx) -> proplists:get_value(uplink_port,Ctx,443).
remote_protocol(Ctx) -> proplists:get_value(uplink_protocol,Ctx,"https").
tls("https")->#{transport => tls};
tls(_) ->#{}.    

uplink_headers(Headers,Ctx)->
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
    
                
