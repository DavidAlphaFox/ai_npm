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

