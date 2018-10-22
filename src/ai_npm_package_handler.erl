-module(ai_npm_package_handler).
-include("ai_npm.hrl").

-export([init/2]).
-define(RESPONSE_HEADERS, [
                           {<<"Content-Type">>, <<"application/octet-stream">>}
                          ]).

init(Req,State)->
    Req2 = case fetch_with_cache(Req) of
               {data,ResHeaders,Data} ->
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
fetch_with_cache(Req)->
    Name = cowboy_req:binding(package,Req),
    Version = cowboy_req:binding(version,Req,undefined),
    Headers = cowboy_req:headers(Req),
    Scheme = cowboy_req:scheme(Req),
    Host = cowboy_req:host(Req),
    Port = cowboy_req:port(Req),
    Path = <<"/",Name/binary>>,
    %%Path = cowboy_req:path(Req),
    CacheProcessor = cache_processor(Name,Version,Path),
    Handler = cache_handler(Scheme,Host,Port,Version,Path),
    case ai_npm_mnesia_cache:try_hit_cache(Path) of
        not_found ->
            fetch_without_cache(Path,maps:to_list(Headers),CacheProcessor,Handler);
        {expired,C}->
            NewHeaders = [{<<"if-none-match">>,C#cache.etag} , {<<"if-modified-since">>,C#cache.last_modified}] ++ maps:to_list(Headers),
            fetch_without_cache(Path,NewHeaders,CacheProcessor,Handler);
        {ok,_C} ->
            Handler()
    end.


cache_processor(Name,Version,Path)-> 
    CachePackage = fun(Body)->
                           Meta = jsx:decode(Body),
                           ID = proplists:get_value(<<"_id">>,Meta),
                           Rev = proplists:get_value(<<"_rev">>,Meta),
                           Key = {ID,Rev},
                           {atomic,ok} = ai_npm_package:add_package(Key,Body),                           
                           Key
                  end,

    fun(Status,ResHeaders,Body)->
            if Status == 304 ->
                    io:format("process not modified respose {~p,~p} ~n",[Name,Version]),
                    ai_npm_mnesia_cache:refresh_headers(Path,ResHeaders),
                    ok;
               Status == 200 ->
                    io:format("process respose {~p,~p} ~n",[Name,Version]),
                    CacheKey = CachePackage(Body),
                    ai_npm_mnesia_cache:add_to_cache(Path,ResHeaders,CacheKey),
                    ok;
               true ->
                    io:format("process respose no_data {~p,~p} ~p~n",[Name,Version,Status]),
                    {no_data,Status,ResHeaders}
            end
    end.

cache_handler(Scheme,Host,Port,Version,Path)->
    ReplaceHostFun = fun(Package)->
                             Dist = proplists:get_value(<<"dist">>,Package),
                             Tarball = proplists:get_value(<<"tarball">>,Dist),
                             %%{http,{undefined,"registry.npmjs.org",80},
                              %%"/react/-/react-0.0.1.tgz",undefined,undefined} 
                             {_S,_H,P,Q,F}= urilib:parse(erlang:binary_to_list(Tarball)),
                             NewTarball = urilib:build({erlang:binary_to_atom(Scheme,utf8),
                                                        {undefined,erlang:binary_to_list(Host),Port},P,Q,F}),
                             NewDist = [{<<"tarball">>,erlang:list_to_binary(NewTarball)}] ++ proplists:delete(<<"tarball">>,Dist), 
                             [{<<"dist">>,NewDist}] ++ proplists:delete(<<"dist">>,Package)
                     end,
    DataFun = fun(C)->
                      Headers = C#cache.headers,
                      CacheKey = C#cache.cache_key,
                      case Version of 
                          undefined ->
                              {atomic,[Package]} = ai_npm_package:find_by_id(CacheKey),
                              Meta = jsx:decode(Package#package.meta),
                              Versions = proplists:get_value(<<"versions">>,Meta),
                              NewVersions = lists:foldl(fun({V,P},Acc)->
                                                                NewP = ReplaceHostFun(P),
                                                                [{V,NewP}| Acc]
                                                        end,[],Versions),
                              NewMeta = [{<<"versions">>,NewVersions}] ++ proplists:delete(<<"versions">>,Meta),
                              {data,Headers,jsx:encode(NewMeta)};
                          _ ->
                              {ok,Meta} = ai_npm_package:find_by_id_version(CacheKey,Version,json),
                              NewMeta = ReplaceHostFun(Meta),
                              {data,Headers,jsx:encode(NewMeta)}
                      end
              end,
    fun()->
            case ai_npm_mnesia_cache:hit_cache(Path) of
                not_found -> not_found;
                C -> DataFun(C)
            end
    end.

fetch_without_cache(Path,Headers,Processor,Handler) ->
    Ctx = [{url,Path},{headers,Headers},{processor,Processor}],
    case ai_idempotence_pool:task_add(pkg,Path,{ai_npm_fetcher,fetch_package,[Ctx]}) of
        {done,ok} -> Handler();
        {done,{no_data,Status,ResHeaders}} -> {no_data,Status,ResHeaders};
        {done,Result}-> Result;
        {error,_Error,_Reason} -> not_found
    end.

clean_headers(Headers)-> 
    Exclued = [<<"set-cookie">>,<<"etag">>,<<"last-modified">>,<<"content-encoding">>,
               <<"cf-cache-status">>,<<"accept-ranges">>,<<"cf-ray">>,<<"expect-ct">>],
    NewHeaders = [{<<"content-encoding">>,<<"identity">>} | lists:filter(fun({Key,_V})-> not lists:member(Key,Exclued) end,Headers)],
    maps:from_list(NewHeaders).

