-module(npm_http_common).
-export([with_etag/3,with_gizp/2,res_headers/1,server_name/1]).
with_etag(Etag,Modified,Headers)->
    List = [
        {ai_http,add_if_none_match,[Etag]},
        {ai_http,add_if_modified_since,[Modified]}
    ],
    ai_lists:run_pipe(List,[Headers]).

with_gizp(Length,Headers)->
    List = [
        {ai_http,add_content_encoding,[gzip]},
        {ai_http,add_content_length,[Length]}
    ],
    ai_lists:run_pipe(List,[Headers]).
res_headers(Headers)-> 
    Exclued = [<<"set-cookie">>,<<"etag">>,<<"last-modified">>,
               <<"cf-cache-status">>,<<"accept-ranges">>,<<"cf-ray">>,<<"expect-ct">>],
    NewHeaders = lists:filter(fun({Key,_V})-> not lists:member(Key,Exclued) end,Headers),
    maps:from_list(NewHeaders).
server_name(Req)->
    Scheme = cowboy_req:scheme(Req),
    Host = cowboy_req:host(Req),
    Port = cowboy_req:port(Req),
    {Scheme,Host,Port}.