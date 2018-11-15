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
            DigestString = ai_string:hash_to_string(Digest,160,lower),
            case npm_tarball_storage:store(TmpFile,DigestString,Scope,Filename) of 
                {ok,FinalFile} -> npm_tarball_mnesia:add({Scope,Name},Version,FinalFile,true);
                Error -> Error
            end;
        Error -> Error
    end.
save_tarball({Scope,Name},[{Filename,Other}])->
	Data = proplists:get_value(?DATA,Other),
	Decode = base64:decode(Data),
    Version = npm_package_common:version(Name,Filename),
    store_tarball(Scope,Name,Version,Filename,Decode).

merge_package(ScopeName,Json)->
    case ai_mnesia_operation:one_or_none(npm_package_mnesia:find_private(ScopeName)) of 
        not_found -> npm_package_mnesia:add_private(ScopeName,jsx:encode(Json));
        Item -> 
            Old = Item#private_package.meta,
            MergedJson = npm_package_common:merge(jsx:decode(Old),Json),
            npm_package_mnesia:add_private(ScopeName,jsx:encode(MergedJson))
    end.

req(?PUT,Req,State)->
	{ok, Data, Req0} = cowboy_req:read_body(Req),
	Json = jsx:decode(Data),
	Tarball = npm_package_common:attachments(Json),
    ScopeName = npm_package_common:scope_name(npm_package_common:name(Json)),
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
                   cowboy_req:reply(200,npm_http_common:res_headers(ResHeaders),Data,Req);
               {data,Status,ResHeaders,Data}->
                   cowboy_req:reply(Status,npm_http_common:res_headers(ResHeaders),Data,Req);
               {no_data,Status,ResHeaders}->
                   cowboy_req:reply(Status,npm_http_common:res_headers(ResHeaders),Req);
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
        {$@,undefined} -> {npm_package_common:scope_name(Scope),undefined};
        {$@,_}-> {{Scope,Name},Version};
        {_N,_V} -> {{undefined,Scope},Name}
    end.

fetch_with_private(Req)->
    {ScopeName,Version} = name_version(Req),
    case ai_mnesia_operation:one_or_none(npm_package_mnesia:find_private(ScopeName)) of 
         not_found -> fetch_with_cache(Req);
         Record -> reply_version(Version,Record#private_package.meta,
            ?PACKAGE_HEADERS,npm_http_common:server_name(Req))
    end.
  
fetch_with_cache(Req)->
    {_ScopeName,Version} = name_version(Req),
    ServerName = npm_http_common:server_name(Req),
    Url = cowboy_req:path(Req),
    Headers = cowboy_req:headers(Req),
    case npm_package_task:run(Url,maps:to_list(Headers)) of
        {hit,CacheKey,ResHeaders}->
            io:format("Cache: [hit] ~p~n",[Url]),
            reply(Url,ResHeaders,ServerName,CacheKey,Version);
        {data,ResHeaders,Data} -> {data,ResHeaders,Data};
        {data,Status,ResHeaders,Data} -> {data,Status,ResHeaders,Data};
        {no_data,Status,ResHeaders} -> {no_data,Status,ResHeaders};
        {error,_Reason}-> not_found
    end.

%%replace_with_host(Scheme,Host,Port,Package)->
 %%   Dist = proplists:get_value(?DIST,Package),
 %%   Tarball = proplists:get_value(?TARBALL,Dist),
    %%{http,{undefined,"registry.npmjs.org",80},
    %%"/react/-/react-0.0.1.tgz",undefined,undefined} 
 %%   {_S,_H,P,Q,F}= urilib:parse(erlang:binary_to_list(Tarball)),
 %%   NewTarball = urilib:build({erlang:binary_to_atom(Scheme,utf8),
 %%                               {undefined,erlang:binary_to_list(Host),Port},P,Q,F}),
 %%   NewDist = [{?TARBALL,erlang:list_to_binary(NewTarball)}] ++ proplists:delete(?TARBALL,Dist), 
 %%   [{?DIST,NewDist}] ++ proplists:delete(?DIST,Package).
reply_version(undefined,Meta,ResHeaders,{_Scheme,_Host,_Port})->
    %%Meta = jsx:decode(Record#package.meta),
    %%Versions = npm_package_common:versions(Meta),
    %%NewVersions = lists:foldl(fun({V,P},Acc)->
    %%                                NewP = replace_with_host(Scheme,Host,Port,P),
    %%                                [{V,NewP}| Acc]
    %%                            end,[],Versions),
    %%NewMeta = [{?VERSIONS,NewVersions}] ++ proplists:delete(?VERSIONS,Meta),
    %%Data = ai_http:encode_body(gzip,jsx:encode(NewMeta)),
    %%Size = erlang:byte_size(Data),
    %%NewHeaders = ResHeaders ++ 
    %%    [{<<"content-length">>,erlang:integer_to_binary(Size)},{<<"content-encoding">>,<<"gzip">>}],
    {data,ResHeaders,Meta};
reply_version(Version,Meta,ResHeaders,{_Scheme,_Host,_Port})->
    Json = jsx:decode(Meta),
    case npm_package_common:version_info(Version,Json) of 
        undefined -> not_found;
        VersionInfo -> 
            %% NewMeta = replace_with_host(Scheme,Host,Port,VersionInfo),
            %% Data = ai_http:encode_body(gzip,jsx:encode(NewMeta)),
            %% Size = erlang:byte_size(Data),
            %% NewHeaders = ResHeaders ++ 
            %%    [{<<"content-length">>,erlang:integer_to_binary(Size)},{<<"content-encoding">>,<<"gzip">>}],
            %%{data,NewHeaders,Data}
            {data,ResHeaders,jsx:encode(VersionInfo)}
    end.
reply(Url,ResHeaders,Http,Name,Version)->
    case ai_mnesia_operation:one_or_none(npm_package_mnesia:find(Name)) of 
        not_found -> 
            ai_http_cache:uncache(Url),
            not_found;
        Record ->
            reply_version(Version,Record#package.meta,ResHeaders,Http)
    end.
