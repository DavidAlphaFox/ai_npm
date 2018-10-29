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
    Tmpfile = erlang:binary_to_list(proplists:get_value(tmp_file,Ctx)),
    Finalfile = erlang:binary_to_list(proplists:get_value(final_file,Ctx)),
    ReqHeaders = npm_fetcher:headers(Ctx),
    {ok, ConnPid} = npm_fetcher:open(Ctx),
    {ok, _Protocol} = gun:await_up(ConnPid),
    StreamRef = gun:get(ConnPid, Url, ReqHeaders),
    case gun:await(ConnPid, StreamRef) of
      {response, fin, Status, Headers} -> {no_data, Status, Headers};
      {response, nofin, Status, Headers} -> download(Status,Headers,TmpFile,FinalFile)
    end.


done(Status, Headers, TmpFile, FinalFile) ->
    case ai_npm_storage:rename(TmpFile, FinalFile) of
        {ok, FinalFile} -> {data, Status, Headers, FinalFile};
		Error -> Error
	end.

download(Status,Headers,TmpFile,FinalFile)->
    case ai_blob_file:open_for_write(TmpFile) of
        {ok, Fd} ->
            case stream_download(Fd, ConnPid, StreamRef) of
				{ok, StreamRef,_Digest} ->
                    done(Status,Headers,Tmpfile,FinalFile);
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
