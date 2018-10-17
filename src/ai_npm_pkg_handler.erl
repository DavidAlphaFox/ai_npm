-module(ai_npm_pkg_handler).
-export([init/2]).
-define(RESPONSE_HEADERS, [
                           {<<"Content-Type">>, <<"application/octet-stream">>},
                           {<<"access-control-allow-origin">>, <<"*">>},
                           {<<"access-control-allow-methods">>, <<"GET">>},
                           {<<"access-control-max-age">>, <<"86400">>}
                          ]).
init(Req, State) ->
    Path = cowboy_req:path(Req),
    io:format("process ~p~n",[Path]),
    Headers = cowboy_req:headers(Req),
    Req2 = case fetch_with_cache(Path,Headers) of
               {ok,{ResHeaders,Body}} ->
                   cowboy_req:reply(200,clean_cookie(ResHeaders),Body,Req);
               {no_data,Status,ResHeaders}->
                   cowboy_req:reply(Status,clean_cookie(ResHeaders),Req);
               not_found ->
                   Body = <<"Internal Server Error">>,
                   cowboy_req:reply(500, maps:from_list(?RESPONSE_HEADERS), Body, Req)
           end,
    io:format("process ~p [done]~n",[Path]),
    {ok, Req2, State}.

fetch_without_cache(Path,Headers) ->
    Ctx = [{url,Path},{headers,Headers}],
    case ai_idempotence_pool:task_add(pkg,{ai_npm_gun,fetch_without_cache,[Ctx]}) of
        {done,ok} -> ai_npm_ets_cache:get(ai_npm_ets_cache, Path);
        {done,{no_data,Status,ResHeaders}} -> {no_data,Status,ResHeaders};
        {error,_Error,_Reason} -> not_found
    end.

clean_cookie(Headers)-> maps:from_list(proplists:delete(<<"set-cookie">>,Headers)).

fetch_with_cache(Path,Headers)->
    case ai_npm_ets_cache:get(ai_npm_ets_cache, Path) of
        not_found -> fetch_without_cache(Path,Headers);
        {ok, {ResHeaders,Body}} ->
            io:format("Path: ~p [Cache HIT]~n",[Path]),
            {ok, {ResHeaders,Body}}
    end.
