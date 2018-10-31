-module(npm_package_api).
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
            DigestString = ai_strings:hash_to_string(Digest,160,lower),
            case npm_tarball_storage:store(TmpFile,DigestString,Scope,Filename) of 
                {ok,FinalFile} -> npm_tarball_mnesia:add({Scope,Name},Version,FinalFile,true);
                Error -> Error
            end;
        Error -> Error
    end.
save_tarball({Scope,Name},[{Filename,Other}])->
	Data = proplists:get_value(?DATA,Other),
	Decode = base64:decode(Data),
    Version = npm_package:version(Name,Filename),
    store_tarball(Scope,Name,Version,Filename,Decode).

merge_package(ScopeName,Json)->
    case ai_mnesia_operation:one_or_none(npm_package_mnesia:find(ScopeName)) of 
        not_found -> npm_package_mnesia:add(ScopeName,jsx:encode(Json),true);
        Item -> 
            Old = Item#package.meta,
            MergedJson = npm_package:merge(jsx:decode(Old),Json),
            npm_package_mnesia:add(ScopeName,jsx:encode(MergedJson),true)
    end.

req(?PUT,Req,State)->
	{ok, Data, Req0} = cowboy_req:read_body(Req),
	Json = jsx:decode(Data),
	Tarball = npm_package:attachments(Json),
    ScopeName = npm_package:scope_name(npm_package:name(Json)),
    {atomic,ok} = save_tarball(ScopeName,Tarball),
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
    Req2 = case fetch_with_private(Req) of
               {data,ResHeaders,Data} ->
                   cowboy_req:reply(200,npm_req:res_headers(ResHeaders),Data,Req);
               {data,Status,ResHeaders,Data}->
                   cowboy_req:reply(Status,npm_req:res_headers(ResHeaders),Data,Req);
               {no_data,Status,ResHeaders}->
                   cowboy_req:reply(Status,npm_req:res_headers(ResHeaders),Req);
               _ ->
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

fetch_with_private(Req)->
    {ScopeName,Version} = name_version(Req),
    case ai_mnesia_operation:one_or_none(npm_package_mnesia:find(ScopeName,true)) of 
         not_found -> fetch_with_cache(Req);
         Record -> 
            io:format("use private package: ~p~n",[ScopeName]),
            reply_version(Version,Record,?PACKAGE_HEADERS,npm_req:server_name(Req))
    end.
  
fetch_with_cache(Req)->
    {ScopeName,Version} = name_version(Req),
    ServerName = npm_req:server_name(Req),
    Url = cowboy_req:path(Req),
    Headers = cowboy_req:headers(Req),
    case ai_http_cache:validate_hit(Url) of 
        not_found -> 
            io:format("Cache: not_found: ~p~n",[ScopeName]),
            fetch_without_cache(Url,maps:to_list(Headers),ServerName,ScopeName,Version);
        {expired,Etag,Modified} -> 
            io:format("Cache: expired: ~p~n",[ScopeName]),
            NewHeaders = npm_req:req_headers(Etag,Modified,Headers),
            fetch_without_cache(Url,NewHeaders,ServerName,ScopeName,Version);
        {hit,CacheKey,ResHeaders}->
            io:format("Cache: hit: ~p~n",[ScopeName]),
            reply(Url,ResHeaders,ServerName,CacheKey,Version)
    end.

replace_with_host(Scheme,Host,Port,Package)->
    Dist = proplists:get_value(?DIST,Package),
    Tarball = proplists:get_value(?TARBALL,Dist),
    %%{http,{undefined,"registry.npmjs.org",80},
    %%"/react/-/react-0.0.1.tgz",undefined,undefined} 
    {_S,_H,P,Q,F}= urilib:parse(erlang:binary_to_list(Tarball)),
    NewTarball = urilib:build({erlang:binary_to_atom(Scheme,utf8),
                                {undefined,erlang:binary_to_list(Host),Port},P,Q,F}),
    NewDist = [{?TARBALL,erlang:list_to_binary(NewTarball)}] ++ proplists:delete(?TARBALL,Dist), 
    [{?DIST,NewDist}] ++ proplists:delete(?DIST,Package).
reply_version(undefined,Record,ResHeaders,{Scheme,Host,Port})->
    Meta = jsx:decode(Record#package.meta),
    Versions = npm_package:versions(Meta),
    NewVersions = lists:foldl(fun({V,P},Acc)->
                                    NewP = replace_with_host(Scheme,Host,Port,P),
                                    [{V,NewP}| Acc]
                                end,[],Versions),
    NewMeta = [{?VERSIONS,NewVersions}] ++ proplists:delete(?VERSIONS,Meta),
    {data,ResHeaders,jsx:encode(NewMeta)};
reply_version(Version,Record,ResHeaders,{Scheme,Host,Port})->
    Meta = jsx:decode(Record#package.meta),
    VersionInfo = npm_package:version_info(Version,Meta),
    case VersionInfo of 
         undefined -> not_found;
         _ -> 
             NewMeta = replace_with_host(Scheme,Host,Port,VersionInfo),
             {data,ResHeaders,jsx:encode(NewMeta)}
    end.
reply(Url,ResHeaders,Http,Name,Version)->
    case ai_mnesia_operation:one_or_none(npm_package_mnesia:find(Name)) of 
        not_found -> 
            ai_http_cache:uncache(Url),
            not_found;
        Record ->
            reply_version(Version,Record,ResHeaders,Http)
    end.

fetch_without_cache(Url,Headers,Http,Name,Version) ->
    Ctx = [{url,Url},{headers,Headers},{cache_key,Name}],
    case ai_idempotence_pool:task_add(pkg,Url,{npm_package_fetcher,do,[Ctx]}) of
        {done,{hit,CacheKey,ResHeaders}} -> reply(Url,ResHeaders,Http,CacheKey,Version);
        {done,Result}-> Result;
        {error,_Error,_Reason} -> not_found
    end.

