-module(ai_npm_pkg_handler).
-export([init/2]).
-define(RESPONSE_HEADERS, [
                           {<<"Content-Type">>, <<"application/octet-stream">>},
                           {<<"access-control-allow-origin">>, <<"*">>},
                           {<<"access-control-allow-methods">>, <<"GET">>},
                           {<<"access-control-max-age">>, <<"86400">>}
                          ]).

init(Req,version)->
    Req2 = case fetch_with_cache(Req) of
               {ok,{ResHeaders,Body}} ->
                   cowboy_req:reply(200,clean_cookie(ResHeaders),Body,Req);
               {no_data,Status,ResHeaders}->
                   cowboy_req:reply(Status,clean_cookie(ResHeaders),Req);
               not_found ->
                   Body = <<"Internal Server Error">>,
                   cowboy_req:reply(500, maps:from_list(?RESPONSE_HEADERS), Body, Req)
           end,
    io:format("process ~p [done]~n",[Path]),
    {ok, Req2, version};
init(Req, tarball) ->
    {ok, Req, tarball}.

fetch_without_cache(Path,Headers) ->
    Ctx = [{url,Path},{headers,Headers},{}],
    case ai_idempotence_pool:task_add(pkg,{ai_npm_gun,fetch_without_cache,[Ctx]}) of
        {done,ok} -> ai_npm_ets_cache:get(ai_npm_ets_cache, Path);
        {done,{no_data,Status,ResHeaders}} -> {no_data,Status,ResHeaders};
        {error,_Error,_Reason} -> not_found
    end.

clean_cookie(Headers)-> maps:from_list(proplists:delete(<<"set-cookie">>,Headers)).

fetch_with_cache(Req)->
    Name = cowboy_req:binding(pkg,Req),
    Version = cowboy_req:binding(version,Req,undefined),
    CacheProcessor = fun(Status,ResHeaders,Body)->
                     end,
    case ai_npm_mnesia:try_hint_cache(Name,Version) of
        not_found ->

                                      

