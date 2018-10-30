-module(npm_api_tarball).
-include("npm_tarball.hrl").

-define(RESPONSE_HEADERS, [
                           {<<"Content-Type">>, <<"application/octet-stream">>}
                          ]).
-export([init/2]).
init(Req,State)->
    Req2 = case fetch_with_private(Req) of
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


fetch_with_private(Req)->
    {Scope,Package,Version,_Tarball,Ctx} = ctx(Req),
    case ai_mnesia_operation:one_or_none(npm_tarball_mnesia:find(Scope,Package,Version,true)) of 
        not_found -> 
            fetch_with_cache(Ctx,Req);
        Record ->
            Path = Record#tarball.path,
            {Offset,Size} = ai_blob_file:data_range(Path), 
            {data,200,[],{sendfile,Offset,Size,Path}}
    end.

ctx(Req)->
    Scope = cowboy_req:binding(scope,Req),
    Package = cowboy_req:binding(package,Req),
    Tarball = cowboy_req:binding(tarball,Req),
    Version  = npm_package:version(Package,Tarball),
    Ctx = [{scope,Scope},{package,Package},{tarball,Tarball},{version,Version}],
    {Scope,Package,Version,Tarball,Ctx}.


fetch_with_cache(Ctx,Req) ->
    Path = cowboy_req:path(Req),
    Headers = cowboy_req:headers(Req),
    case ai_http_cache:validate_hit(Path) of 
        not_found ->
            fetch_without_cache(Path,maps:to_list(Headers),Ctx);
        {expired,Etag,Modified} ->
            NewHeaders = req_headers(Etag,Modified,Headers),
            fetch_without_cache(Path,NewHeaders,Ctx);
        {hit,CacheKey,ResHeaders}
            reply(CacheKey,Path,ResHeaders)
    end.
fetch_without_cache(Path,Headers,Ctx) ->
    FetcherCtx = [{url,Path},{headers,Headers}] ++ Ctx, 
    case ai_idempotence_pool:task_add(pkg,Path,{npm_tarball_fetcher,do,[Ctx]}) of
        {done,{data,ResHeaders,File}} -> 
            {Offset,Size} = ai_blob_file:data_range(File),
            {data,ResHeaders,{sendfile,Offset,Size,File}};
        {done,{data,Status,ResHeaders,Data}} -> {data,Status,ResHeaders,Data};
        {done,{no_data,Status,ResHeaders}} -> {no_data,Status,ResHeaders};
        {done,{hit,CacheKey,ResHeaders}} -> reply(CacheKey,ResHeaders)
        _ -> not_found
    end.


reply({Scope,Package,Version},Url,ResHeaders)->
   case ai_mnesia_operation:one_or_none(npm_tarball_mnesia:find(Scope,Package,Version)) of 
        not_found -> 
            ai_http_cache:uncache(url),
            not_found
        Record ->
            Path = Record#tarball.path,
            {Offset,Size} = ai_blob_file:data_range(Path), 
            {data,200,ResHeaders,{sendfile,Offset,Size,Path}}
    end.

clean_headers(Headers)-> 
    Exclued = [<<"set-cookie">>,<<"etag">>,<<"last-modified">>,<<"content-encoding">>,
               <<"cf-cache-status">>,<<"accept-ranges">>,<<"cf-ray">>,<<"expect-ct">>],
    NewHeaders = [{<<"content-encoding">>,<<"identity">>} | lists:filter(fun({Key,_V})-> not lists:member(Key,Exclued) end,Headers)],
    maps:from_list(NewHeaders).
