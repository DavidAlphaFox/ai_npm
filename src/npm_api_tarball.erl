-module(npm_api_tarball).
-include("npm_tarball.hrl").

-define(RESPONSE_HEADERS, [
                           {<<"Content-Type">>, <<"application/octet-stream">>}
                          ]).
-export([init/2]).
init(Req,State)->
    Req2 = case fetch_with_cache(Req) of
               {data,ResHeaders,Data}->
                   cowboy_req:reply(200,clean_headers(ResHeaders),Data,Req);
               {data,Status,ResHeaders,Data}->
                   cowboy_req:reply(Status,clean_headers(ResHeaders),Data,Req);
               {no_data,Status,ResHeaders}->
                   cowboy_req:reply(Status,clean_headers(ResHeaders),Req);
               not_found ->
                   Body = <<"Internal Server Error">>,
                   cowboy_req:reply(500, maps:from_list(?RESPONSE_HEADERS), Body, Req)
           end,
    {ok, Req2, State}.

version(Package,Tarball)->
    Tail = string:prefix(Tarball,<<Package,"-">>),
    Suffix = string:find(Tail,".",trailing),
    SLen = erlang:byte_size(Suffix),
    TLen = erlang:byte_size(Tail),
    string:slice(Tail,0,TLen - SLen).

fetch_with_private(Req)->
    Scope = cowboy_req:binding(scope,Req),
    Package = cowboy_req:binding(package,Req),
    Tarball = cowboy_req:binding(tarball,Req),
    Version  = version(Package,Tarball),
    case ai_mnesia_operation:one_or_none(npm_tarball_mnesia(Scope,Package,Version)) of 
        not_found -> fetch_with_cache(Req),
        Record ->
            Path = Record#tarball.path,
            {Offset,Size} = ai_blob_file:data_range(Path), 
            {data,200,[],{sendfile,Offset,Size,Path}}
    end.

fetch_with_cache(Req) ->
    Tarball = cowboy_req:binding(tarball,Req),
    Path = cowboy_req:path(Req),
    Headers = cowboy_req:headers(Req),
    io:format("process tarball path ~p~n",[Path]),
    case ai_npm_mnesia_cache:try_hit_cache(Path) of
        not_found ->
            fetch_without_cache(Path,maps:to_list(Headers),Tarball);
        {expired,C}->
            NewHeaders = [{<<"if-none-match">>,C#cache.etag} , {<<"if-modified-since">>,C#cache.last_modified}] ++ maps:to_list(Headers),
            fetch_without_cache(Path,NewHeaders,Tarball);
        {ok,C} ->
            File = C#cache.cache_key,
            ResHeaders = C#cache.headers,
            {Offset,Size} = ai_blob_file:data_range(File),
            {data,ResHeaders,{sendfile,Offset,Size,File}}
    end.
fetch_without_cache(Path,Headers,Tarball) ->
    Ctx = [{url,Path},{headers,Headers},{tarball,Tarball}],
    case ai_idempotence_pool:task_add(pkg,Path,{ai_npm_fetcher,fetch_tarball,[Ctx]}) of
        {done,{data,Status,ResHeaders,File}} -> 
            {Offset,Size} = ai_blob_file:data_range(File),
            ai_npm_mnesia_cache:add_to_cache(Path,ResHeaders,File),
            {data,Status,ResHeaders,{sendfile,Offset,Size,File}};
        {done,{no_data,Status,ResHeaders}} -> 
            if 
                Status == 304 ->
                    ai_npm_mnesia_cache:refresh_headers(Path,ResHeaders),
                    C = ai_npm_mnesia_cache:hit_cache(Path),
                    File = C#cache.cache_key,
                    {Offset,Size} = ai_blob_file:data_range(File),
                    {data,ResHeaders,{sendfile,Offset,Size,File}};
                true ->
                    {no_data,Status,ResHeaders}
            end;
        _ -> not_found
    end.

clean_headers(Headers)-> 
    Exclued = [<<"set-cookie">>,<<"etag">>,<<"last-modified">>,<<"content-encoding">>,
               <<"cf-cache-status">>,<<"accept-ranges">>,<<"cf-ray">>,<<"expect-ct">>],
    NewHeaders = [{<<"content-encoding">>,<<"identity">>} | lists:filter(fun({Key,_V})-> not lists:member(Key,Exclued) end,Headers)],
    maps:from_list(NewHeaders).
