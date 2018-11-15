-module(npm_http_common).
-export([req_headers/3,res_headers/1,server_name/1]).

req_headers(undefined,undefined,Headers) -> Headers;
req_headers(Etag,undefined,Headers) ->  [{<<"if-none-match">>,Etag}] ++ Headers;
req_headers(undefined,Modified,Headers) -> [{<<"if-modified-since">>,Modified}] ++ Headers;
req_headers(Etag,Modified,Headers)->
    [{<<"if-none-match">>,Etag} , {<<"if-modified-since">>,Modified}] ++ Headers.

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