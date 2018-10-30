-module(npm_req).
-export([req_headers/3,res_headers/1]).

req_headers(undefined,undefined,Headers) -> maps:to_list(Headers);
req_headers(Etag,undefined,Headers) ->  [{<<"if-none-match">>,Etag}] ++ maps:to_list(Headers);
req_headers(undefined,Modified,Headers) -> [{<<"if-modified-since">>,Modified}] ++ maps:to_list(Headers);
req_headers(Etag,Modified,Headers)->
    [{<<"if-none-match">>,Etag} , {<<"if-modified-since">>,Modified}] ++ maps:to_list(Headers).


res_headers(Headers)-> 
    Exclued = [<<"set-cookie">>,<<"etag">>,<<"last-modified">>,<<"content-encoding">>,
               <<"cf-cache-status">>,<<"accept-ranges">>,<<"cf-ray">>,<<"expect-ct">>],
    NewHeaders = [{<<"content-encoding">>,<<"identity">>} | lists:filter(fun({Key,_V})-> not lists:member(Key,Exclued) end,Headers)],
    maps:from_list(NewHeaders).
