-module(npm_tarball_api).
-include("npm_tarball.hrl").
-include("req.hrl").

-define(RESPONSE_HEADERS, [
                           {<<"Content-Type">>, <<"application/octet-stream">>}
                          ]).
-export([init/2]).
init(Req,State)->
    Req2 = case fetch_with_private(Req) of
               {data,ResHeaders,Data}->
                   cowboy_req:reply(200,npm_req:res_headers(ResHeaders),Data,Req);
               {data,Status,ResHeaders,Data}->
                   cowboy_req:reply(Status,npm_req:res_headers(ResHeaders),Data,Req);
               {no_data,Status,ResHeaders}->
                   cowboy_req:reply(Status,npm_req:res_headers(ResHeaders),Req);
               not_found ->
                   Body = <<"Internal Server Error">>,
                   cowboy_req:reply(500, maps:from_list(?RESPONSE_HEADERS), Body, Req)
           end,
    {ok, Req2, State}.


fetch_with_private(Req)->
    {Scope,Package,Version,_Tarball,Ctx} = ctx(Req),
    case ai_mnesia_operation:one_or_none(npm_tarball_mnesia:find({Scope,Package},Version,true)) of 
        not_found ->  fetch_with_cache(Ctx,Req);
        Record -> reply_tarball(Record,?TARBALL_HEADERS)
    end.

ctx(Req)->
    Scope = cowboy_req:binding(scope,Req),
    Package = cowboy_req:binding(package,Req),
    Tarball = cowboy_req:binding(tarball,Req),
    io:format("npm tarball: ~p ~p ~n",[Package,Tarball]),
    Version  = npm_package:version(Package,Tarball),
    Ctx = [{scope,Scope},{package,Package},{tarball,Tarball},{version,Version}],
    {Scope,Package,Version,Tarball,Ctx}.


fetch_with_cache(Ctx,Req) ->
    Url = cowboy_req:path(Req),
    Headers = cowboy_req:headers(Req),
    case ai_http_cache:validate_hit(Url) of 
        not_found ->
            fetch_without_cache(Url,maps:to_list(Headers),Ctx);
        {expired,Etag,Modified} ->
            NewHeaders = npm_req:req_headers(Etag,Modified,Headers),
            fetch_without_cache(Url,NewHeaders,Ctx);
        {hit,CacheKey,ResHeaders}->
            reply(Url,ResHeaders,CacheKey)
    end.

fetch_without_cache(Url,Headers,Ctx) ->
    FetcherCtx = [{url,Url},{headers,Headers}] ++ Ctx, 
    case ai_idempotence_pool:task_add(pkg,Url,{npm_tarball_fetcher,do,[FetcherCtx]}) of
        {done,{hit,CacheKey,ResHeaders}} -> reply(Url,ResHeaders,CacheKey);
        {done,{error,_Any}} -> not_found;
        {done,Result} -> Result;
        _ -> not_found
    end.

reply_tarball(Record,ResHeaders)->
    Path = Record#tarball.path,
    {Offset,Size} = ai_blob_file:data_range(Path),
    NewResHeaders = ResHeaders ++ [{<<"content-length">>,erlang:integer_to_binary(Size)}], 
    {data,NewResHeaders,{sendfile,Offset,Size,Path}}.

reply(Url,ResHeaders,{Scope,Package,Version})->
   case ai_mnesia_operation:one_or_none(npm_tarball_mnesia:find({Scope,Package},Version)) of 
        not_found -> 
            ai_http_cache:uncache(Url),
            not_found;
        Record ->
            reply_tarball(Record,ResHeaders)
    end.
