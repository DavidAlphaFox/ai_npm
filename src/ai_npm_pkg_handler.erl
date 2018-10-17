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
    Req2 = case fetch_without_cache(Path,Headers) of
               {done,Result} ->
                   process_result(Result,Req);
               {error,_Error,_Reason}->
                   Body = <<"Internal Server Error">>,
                   cowboy_req:reply(500, maps:from_list(?RESPONSE_HEADERS), Body, Req)
           end,
    io:format("process ~p [done]~n",[Path]),
    {ok, Req2, State}.
fetch_without_cache(Path,Headers) ->
    Ctx = [{url,Path},{headers,Headers}],
    ai_idempotence_pool:task_add(pkg,{ai_npm_gun,fetch_without_cache,[Ctx]}).
    

process_result(Result,Req)->
    case Result of
        {no_data,Status,Headers}-> 
            cowboy_req:reply(Status,replace_cookie(Headers),Req);
        {data,Status,Headers,Body}->
            cowboy_req:reply(Status,replace_cookie(Headers),Body,Req)
    end.
replace_cookie(Headers)->
    Cookie = proplists:get_value(<<"set-cookie">>,Headers,<<"">>),
    NewHeaders = proplists:delete(<<"set-cookie">>,Headers),
    maps:from_list([{<<"set-cookie">>,[Cookie]}|NewHeaders]).
