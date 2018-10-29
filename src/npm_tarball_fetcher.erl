-module(npm_tarball_fetcher).
-export([do/1]).
do(Ctx)->
    Url = proplists:get_value(url,Ctx),
    Fun = fun(CacheHint) ->
        do_on_cache(CacheHint,Ctx)
    end,
    npm_cache:run_cache(Url,{Fun,[]}).
do_on_cache({hit,CacheKey,Headers})-> {hit,CacheKey,Headers};
do_on_cache(_,Ctx)->
    Url = proplists:get_value(url, Ctx),
    Scope = proplists:get_value(scope,Ctx),
    Tarball = proplists:get_value(tarball,Ctx),
    ReqHeaders = npm_fetcher:headers(Ctx),
    {ok, ConnPid} = npm_fetcher:open(Ctx),
    {ok, _Protocol} = gun:await_up(ConnPid),
    StreamRef = gun:get(ConnPid, Url, ReqHeaders),
    case gun:await(ConnPid, StreamRef) of
      {response, fin, Status, Headers} -> {no_data, Status, Headers};
      {response, nofin, Status, Headers} -> download(Status,Headers,Scope,Tarball)
    end.


done(Status, Headers, TmpFile,Scope,Tarball,Digest) ->
    case npm_tarball_storage:store(TmpFile,Scope,Tarball,Digest) of
        {ok, FinalFile} -> {data, Status, Headers, FinalFile};
		Error -> Error
	end.

download(Status,Headers,Scope,Tarball)->
    TmpFile = npm_tarball_storage:tmpfile(Scope,Tarball),
    case ai_blob_file:open_for_write(TmpFile) of
        {ok, Fd} ->
            case stream_download(Fd, ConnPid, StreamRef) of
				{ok, StreamRef,Digest} ->
                    DigestString = ai_strings:hash_to_string(Digest,160,lower),
                    done(Status,Headers,Tmpfile,Scope,Tarball,DigestString);
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
			gun:close(Socket), {error, StreamRef, timeout}
    end.
