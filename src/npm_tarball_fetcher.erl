-module(npm_tarball_fetcher).
-export([do/1]).
do(Ctx)->
    Url = proplists:get_value(url,Ctx),
    Fun = fun(CacheHint) ->
        do_on_cache(CacheHint,Ctx)
    end,
    npm_cache:run_cache(Url,{Fun,[]}).
do_on_cache({hit,CacheKey,Headers},_Ctx)-> {hit,CacheKey,Headers};
do_on_cache(_,Ctx)->
    Url = proplists:get_value(url, Ctx),
    Tar = tarball(Ctx),
    ReqHeaders = npm_fetcher:headers(Ctx),
    {ok, ConnPid} = npm_fetcher:open(Ctx),
    {ok, _Protocol} = gun:await_up(ConnPid),
    StreamRef = gun:get(ConnPid, Url, ReqHeaders),
    case gun:await(ConnPid, StreamRef) of
      {response, fin, Status, ResHeaders} -> cache(no_data,Url,ResHeaders,Status,Tar);
      {response, nofin, Status, ResHeaders} -> 
        if  Status  == 200 ->  download(ConnPid,StreamRef,Url,ResHeaders,Tar);
            true ->  
                {ok,Data} = gun:await_body(ConnPid,StreamRef),
                {data,Status,ResHeaders,Data}
        end
    end.


tarball(Ctx)->
    Scope = proplists:get_value(scope,Ctx),
    Version = proplists:get_value(version,Ctx),
    Package = proplists:get_value(package,Ctx),
    Tarball = proplists:get_value(tarball,Ctx),
    {Scope,Package,Version,Tarball}.

cache(no_data,Url,ResHeaders,Status,Tar)->
    {Scope,Package,Version,_Tarball} = Tar,
    if 
        Status == 304 ->
            ai_http_cache:cache(Url,ResHeaders),
            {hit,{Scope,Package,Version},ResHeaders};
        true ->
            {no_data,Status,ResHeaders}
    end;
cache(data,Url,Headers,Path,Tar)->
    {Scope,Package,Version,_Tarball} = Tar,
    npm_tarball_mnesia:add({Scope,Package},Version,Path),
    ai_http_cache:cache(Url,{Scope,Package,Version},Headers).

done(ResHeaders,Url,TmpFile,Digest,Tar) ->
    {Scope,Package,Version,Tarball} = Tar,
    case npm_tarball_storage:store(TmpFile,Digest,Scope,Tarball) of
        {ok, FinalFile} -> 
            cache(data,Url,ResHeaders,FinalFile,Tar),
            {hit,{Scope,Package,Version},ResHeaders};
		Error -> Error
	end.

download(ConnPid,StreamRef,Url,ResHeaders,Tar)->
    {Scope,_Package,_Version,Tarball} = Tar,
    io:format("download tarball ~p~n",[Tar]),
    Tmpfile = npm_tarball_storage:tmpfile(Scope,Tarball),
    case ai_blob_file:open_for_write(Tmpfile) of
        {ok, Fd} ->
            case stream_download(Fd, ConnPid, StreamRef) of
				{ok, StreamRef,Digest} ->
                    DigestString = ai_strings:hash_to_string(Digest,160,lower),
                    done(ResHeaders,Url,Tmpfile,DigestString,Tar);
				{error, StreamRef, Reason} -> {error, Reason}
			end;
		Error -> Error
	end.
    

stream_download(Fd, ConnPid, StreamRef) ->
    MRef = erlang:monitor(process, ConnPid),
    try stream_download(Fd, ConnPid, StreamRef, MRef) catch
      _Error:Reason -> {error, StreamRef, Reason}
    after
      erlang:demonitor(MRef)
    end.

stream_download(OFile, Socket, StreamRef, MRef) ->
    receive
		{gun_data, Socket, StreamRef, nofin, Data} ->
	  		{ok,NewFd} = ai_blob_file:write(OFile, Data),
	  		stream_download(NewFd, Socket, StreamRef, MRef);
      	{gun_data, Socket, StreamRef, fin, Data} ->
	  		{ok,NewFd} = ai_blob_file:write(OFile, Data),
	  		{ok,_NewFd2,Digest} = ai_blob_file:close(NewFd),
	  		{ok, StreamRef,Digest};
      	{'DOWN', MRef, process, Socket, Reason} ->
	  		{error, StreamRef, Reason}
    	after 10000 ->
			gun:close(Socket), 
            {error, StreamRef, timeout}
    end.
