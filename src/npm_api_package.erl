-module(npm_api_package).
-include("npm_package.hrl").

-export([init/2]).
-define(RESPONSE_HEADERS, [
                           {<<"Content-Type">>, <<"application/octet-stream">>}
                          ]).

init(Req,State)->
	Method = cowboy_req:method(Req),
	handle_req(Method,Req,State).
handle_req(<<"PUT">>,Req,State)->
	{ok, Data, Req0} = cowboy_req:read_body(Req),
	Json = jsx:decode(Data),
	Tarball = npm_package:attachments(Json),
    ScopeName = npm_package:scope_name(Json),
    {ok,_FinalFile} = save_tarball(ScopeName,Tarball),
    Req1 = 
        case npm_package:merge_package(Json) of
            {atomic,ok} ->
                Res = jsx:encode([{<<"success">>,true}]),
                cowboy_req:reply(201,#{<<"content-type">> => <<"application/json">>},Res,Req0);
            _Error ->
                Res = jsx:encode([{<<"success">>,false}]),
                cowboy_req:reply(404,#{<<"content-type">> => <<"application/json">>},Res,Req0)
        end,
		{ok,Req1,State};	
handle_req(<<"GET">>,Req,State)->
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

save_tarball({Scope,_Name} = ScopeName,[{Filename1,Other}|Rest])->
    Filename = erlang:binary_to_list(Filename1),
	Data = proplists:get_value(<<"data">>,Other),
	Decode = base64:decode(Data),
    TmpFile = npm_tarball_storage:tmpfile(Scope,Filename),
    Fun = fun(Dir)->
                case ai_blob_file:open_for_write(TmpFile) of    
                    {ok,Fd} -> 
                        ai_blob_file:write(Fd,Decode),
                        {ok,_NewFd,Digest} = ai_blob_file:close(Fd),
                    Error -> Error
                end
            end,
    save_tarball(ScopeName,Rest).

fetch_with_cache(Req)->
    Name = cowboy_req:binding(package,Req),
    Version = cowboy_req:binding(version,Req,undefined),
    Version2 = cowboy_req:binding(scope_version,Req,undefined),
    Headers = cowboy_req:headers(Req),
    Scheme = cowboy_req:scheme(Req),
    Host = cowboy_req:host(Req),
    Port = cowboy_req:port(Req),
    {Path,RelVersion} = 
        case {binary:first(Name),Version} of
            {$@,undefined} -> {cowboy_req:path(Req),Version};
            {$@,_}->
                P = http_uri:encode(<<Name/binary,"/",Version/binary>>),
                {<<"/",P/binary>>,Version2};
            {_,undefined}-> {cowboy_req:path(Req),Version};
            _ -> {<<"/",Name/binary>>,Version}
        end,
    io:format("process package path: ~p~n",[Path]),
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


cache_processor(Path)-> 
    CachePackage = fun(Body)->
                           Meta = jsx:decode(Body),
                           case proplists:get_value(<<"name">>,Meta) of
                               undefined -> undefined;
                               ID ->
                                   {atomic,ok} = ai_npm_package:add_package(ID,Body),
                                   ID
                           end
                  end,

    fun(Status,ResHeaders,Body)->
            if Status == 304 ->
                    ai_npm_mnesia_cache:refresh_headers(Path,ResHeaders),
                    ok;
               Status == 200 ->
                    case CachePackage(Body) of
                        undefined ->
                            {data,Status,ResHeaders,Body};
                        CacheKey->                                
                            ai_npm_mnesia_cache:add_to_cache(Path,ResHeaders,CacheKey),
                            ok
                    end;
               true ->
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

