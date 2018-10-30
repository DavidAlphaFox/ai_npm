-module(npm_api_package).
-include("npm_package.hrl").
-include("req.hrl").

-export([init/2]).
-define(RESPONSE_HEADERS, [
                           {<<"Content-Type">>, <<"application/octet-stream">>}
                          ]).

init(Req,State)->
	Method = cowboy_req:method(Req),
	req(Method,Req,State).


store_tarball(Scope,Name,Version,Filename,Data)->
    TmpFile = npm_tarball_storage:tmpfile(Scope,Filename),
    case ai_blob_file:open_for_write(TmpFile) of    
        {ok,Fd} -> 
            ai_blob_file:write(Fd,Data),
            {ok,_NewFd,Digest} = ai_blob_file:close(Fd),
            case npm_tarball_storage:store(TmpFile,Digest,Scope,Filename) of 
                {ok,FinalFile} -> npm_tarball_mnesia:add({Scope,Name},Version,FinalFile,true);
                Error -> Error
        Error -> Error
    end.
store_tarball({Scope,Name} = ScopeName,[{Filename,Other}|Rest])->
	Data = proplists:get_value(?DATA,Other),
	Decode = base64:decode(Data),
    Version = npm_package:version(Name,Filename),
    store_tarball(Scope,Name,Version,Filename,Decode),
    save_tarball(ScopeName,Rest).

merge_package(ScopeName,Json)->
    case ai_mnesia_operation:one_or_none(npm_package_mnesia:find(ScopeName)) of 
        not_found -> npm_package_mnesia:add(ScopeName,jsx:encode(Json),true);
        Item -> 
            MergedJson = npm_package:merge_package(jsx:decode(Item),Json),
            npm_package_mnesia:add(ScopeName,jsx:encode(MergedJson))
    end.

req(?PUT,Req,State)->
	{ok, Data, Req0} = cowboy_req:read_body(Req),
	Json = jsx:decode(Data),
	Tarball = npm_package:attachments(Json),
    ScopeName = npm_package:scope_name(npm_package:name(Json)),
    {atomic,ok} = store_tarball(ScopeName,Tarball),
    Req1 = 
        case merge_package(ScopeName,proplists:delete(?ATTACHMENTS,Json)) of 
            {atomic,ok} -> 
                Res = jsx:encode([{<<"success">>,true}]),
                cowboy_req:reply(201,#{<<"content-type">> => <<"application/json">>},Res,Req0);
            _Error ->
                Res = jsx:encode([{<<"success">>,false}]),
                cowboy_req:reply(404,#{<<"content-type">> => <<"application/json">>},Res,Req0)
        end,
	{ok,Req1,State};	

req(?GET,Req,State)->
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



name_version(Req)->
    Scope = cowboy_req:binding(scope,Req),
    Name = cowboy_req:binding(package,Req,undefined),
    Version = cowboy_req:binding(version,Req,undefined),
    case {binary:first(Scope),Name} of
        {$@,undefined} -> {npm_package:scope_name(Scope),undefined};
        {$@,_}-> {{Scope,Name},Version};
        {_N,_V} -> {{undefined,Scope},Name}
    end.
fetch_with_cache(Req)->
    {ScopeName,Version} = name_version(Req),
    Path = cowboy_req:path(Req),
    Headers = cowboy_req:headers(Req),
    CacheProcessor = cache_processor(Path),
    Handler = cache_handler(Scheme,Host,Port,RelVersion,Path),
    case ai_npm_mnesia_cache:try_hit_cache(Path) of
        not_found ->
            fetch_without_cache(Path,maps:to_list(Headers),CacheProcessor,Handler);
        {expired,C}->
            NewHeaders = [{<<"if-none-match">>,C#cache.etag} , {<<"if-modified-since">>,C#cache.last_modified}] ++ maps:to_list(Headers),
            fetch_without_cache(Path,NewHeaders,CacheProcessor,Handler);
        {ok,_C} ->
            Handler()
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
                              {atomic,[Package]} = ai_npm_package:find_by_name(CacheKey),
                              Meta = jsx:decode(Package#package.meta),
                              Versions = proplists:get_value(<<"versions">>,Meta),
                              NewVersions = lists:foldl(fun({V,P},Acc)->
                                                                NewP = ReplaceHostFun(P),
                                                                [{V,NewP}| Acc]
                                                        end,[],Versions),
                              NewMeta = [{<<"versions">>,NewVersions}] ++ proplists:delete(<<"versions">>,Meta),
                              {data,Headers,jsx:encode(NewMeta)};
                          _ ->
                              {atomic,Meta} = ai_npm_package:find_by_name_version(CacheKey,Version,json),
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

