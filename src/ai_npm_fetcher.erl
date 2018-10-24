-module(ai_npm_fetcher).

-export([fetch_package/1, fetch_tarball/1]).

fetch_package(Ctx) ->
    {ok, ConnPid} = ai_npm_gun:open(Ctx),
    {ok, _Protocol} = gun:await_up(ConnPid),
    Url = proplists:get_value(url, Ctx),
    ReqHeaders =
	ai_npm_gun:headers(proplists:get_value(headers, Ctx,[]),Ctx),
    CacheProcessor = proplists:get_value(processor, Ctx),
    StreamRef = gun:get(ConnPid, Url, ReqHeaders),
    case gun:await(ConnPid, StreamRef) of
		{response, fin, Status, Headers} ->
	  		CacheProcessor(Status, Headers, no_data);
      	{response, nofin, Status, Headers} ->
	  		{ok, Body} = gun:await_body(ConnPid, StreamRef),
	  		Encoder = proplists:get_value(<<"content-encoding">>,Headers),
	  		Body2 = ai_npm_gun:decode_body(Encoder, Body),
	  		CacheProcessor(Status, Headers, Body2)
    end.

fetch_tarball(Ctx) ->
    {ok, ConnPid} = ai_npm_gun:open(Ctx),
    {ok, _Protocol} = gun:await_up(ConnPid),
    Url = proplists:get_value(url, Ctx),
    Tarball =
	erlang:binary_to_list(proplists:get_value(tarball,
						  Ctx)),
    DefaultStorage = ai_file:priv_dir(ai_npm, "storage"),
    TmpDir = ai_file:priv_dir(ai_npm, "tmp"),
    Storage = proplists:get_value(storage, Ctx,
				  DefaultStorage),
    ReqHeaders =
	ai_npm_gun:headers(proplists:get_value(headers, Ctx,
					       []),
			   Ctx),
    StreamRef = gun:get(ConnPid, Url, ReqHeaders),
    Final = fun (Status, Headers, TmpFile, FinalFile) ->
		    case ai_npm_storage:rename(TmpFile, FinalFile) of
		      {ok, FinalFile} -> {data, Status, Headers, FinalFile};
		      Error -> Error
		    end
	    end,
    Downloader = fun (Status, Headers, Dir) ->
			 TmpFile = filename:join([Dir, Tarball]),
			 FinalFile = filename:join([Storage, Tarball]),
			 case ai_file:open_for_write(TmpFile) of
			   {ok, Fd} ->
			       case download_tarball(Fd, ConnPid, StreamRef) of
				 {ok, StreamRef} ->
				     Final(Status, Headers, TmpFile, FinalFile);
				 {error, StreamRef, Reason} -> {error, Reason}
			       end;
			   Error -> Error
			 end
		 end,
    case gun:await(ConnPid, StreamRef) of
      {response, fin, Status, Headers} ->
	  {no_data, Status, Headers};
      {response, nofin, Status, Headers} ->
	  ai_tmp:run_with_tmp("ai_npm_downloader",
			      [{clean, true}, {path, TmpDir}],
			      {Downloader, [Status, Headers]})
    end.

download_tarball(Fd, ConnPid, StreamRef) ->
    MRef = erlang:monitor(process, ConnPid),
    try download_tarball(Fd, ConnPid, StreamRef, MRef) catch
      _Error:Reason -> {error, StreamRef, Reason}
    after
      erlang:demonitor(MRef)
    end.

download_tarball(OFile, Socket, StreamRef, MRef) ->
    receive
		{gun_data, Socket, StreamRef, nofin, Data} ->
	  		ok = file:write(OFile, Data),
	  		download_tarball(OFile, Socket, StreamRef, MRef);
      	{gun_data, Socket, StreamRef, fin, Data} ->
	  		ok = file:write(OFile, Data),
	  		ok = file:sync(OFile),
	  		ok = file:close(OFile),
	  		{ok, StreamRef};
      	{'DOWN', MRef, process, Socket, Reason} ->
	  		{error, StreamRef, Reason}
    	after 10000 ->
			gun:close(Socket), {error, StreamRef, timeout}
    end.
