-module(ai_npm_pkg_handler).
-include("ai_npm.hrl").

-export([init/2]).
-define(RESPONSE_HEADERS, [
                           {<<"Content-Type">>, <<"application/octet-stream">>}
                          ]).

init(Req,version)->
    Req2 = case fetch_with_cache(Req) of
               {ok,ResHeaders,C} ->
                   cowboy_req:reply(200,clean_headers(ResHeaders),C#package.meta,Req);
               {no_data,Status,ResHeaders}->
                   cowboy_req:reply(Status,clean_headers(ResHeaders),Req);
               not_found ->
                   Body = <<"Internal Server Error">>,
                   cowboy_req:reply(500, maps:from_list(?RESPONSE_HEADERS), Body, Req)
           end,
    {ok, Req2, version};
init(Req, tarball) ->
    {ok, Req, tarball}.

fetch_without_cache(Path,Headers,Processor,Handler) ->
    Ctx = [{url,Path},{headers,Headers},{processor,Processor}],
    case ai_idempotence_pool:task_add(pkg,{ai_npm_gun,fetch_without_cache,[Ctx]}) of
        {done,ok} -> Handler();
        {done,{no_data,Status,ResHeaders}} -> {no_data,Status,ResHeaders};
        {error,_Error,_Reason} -> not_found
    end.

clean_headers(Headers)-> 
    Exclued = [<<"set-cookie">>,<<"etag">>,<<"last-modified">>],
    NewHeaders = lists:filter(fun({Key,_V})-> not lists:member(Key,Exclued) end,Headers),
    maps:from_list(NewHeaders).


fetch_with_cache(Req)->
    Name = cowboy_req:binding(pkg,Req),
    Version = cowboy_req:binding(version,Req,undefined),
    Headers = cowboy_req:headers(Req),
    Path = cowboy_req:path(Req),
    CacheProcessor = fun(Status,ResHeaders,Body)->
                             if Status == 304 ->
                                     io:format("process not modified respose {~p,~p} ~n",[Name,Version]),
                                     ai_npm_mnesia:refresh_headers(Name,Version,ResHeaders),
                                     ok;
                                Status == 200 ->
                                     io:format("process respose {~p,~p} ~n",[Name,Version]),
                                     ok = ai_npm_mnesia:add_data({Name,Version},Name,Body),
                                     ai_npm_mnesia:add_to_cache(Name,Version,ResHeaders,{Name,Version}),
                                     ok;
                                true ->
                                     {no_data,Status,ResHeaders}
                             end
                     end,
    Handler = fun()->
                      {ok,C} = ai_npm_mnesia:try_hint_cache(Name,Version),
                      {ok,Data} = ai_npm_mnesia:retrive_data(C#package_cache.cache_key),
                      {ok,C#package_cache.headers,Data}
                          
              end,
    case ai_npm_mnesia:try_hint_cache(Name,Version) of
        not_found ->
            fetch_without_cache(Path,maps:to_list(Headers),CacheProcessor,Handler);
        {expired,C}->
            NewHeaders = [{<<"if-none-match">>,C#package_cache.etag} | maps:to_list(Headers)],
            fetch_without_cache(Path,NewHeaders,CacheProcessor,Handler);
        {ok,C} ->
            io:format("cache hit {~p,~p} ~p~n",[Name,Version,C]),
            {ok,Data} =ai_npm_mnesia:retrive_data(C#package_cache.cache_key),
            {ok,C#package_cache.headers,Data}
    end.

