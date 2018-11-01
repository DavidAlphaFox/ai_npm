-module(npm_req).
-export([req_headers/3,res_headers/1,server_name/1]).
-export([encode_http/2,decode_http/2]).

req_headers(undefined,undefined,Headers) -> maps:to_list(Headers);
req_headers(Etag,undefined,Headers) ->  [{<<"if-none-match">>,Etag}] ++ maps:to_list(Headers);
req_headers(undefined,Modified,Headers) -> [{<<"if-modified-since">>,Modified}] ++ maps:to_list(Headers);
req_headers(Etag,Modified,Headers)->
    [{<<"if-none-match">>,Etag} , {<<"if-modified-since">>,Modified}] ++ maps:to_list(Headers).


decode_http(<<"gzip">>,Body)->zlib:gunzip(Body);
decode_http(_,Body) -> Body.
-spec encode_http(atom(),binary())->binary().
encode_http(gzip,Body) -> zlib:gzip(Body);
encode_http(_,Body) -> Body.

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