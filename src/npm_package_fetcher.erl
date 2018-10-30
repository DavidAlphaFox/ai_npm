-module(npm_package_fetcher).
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
    ReqHeaders =npm_fetcher:headers(Ctx),
    {ok, ConnPid} = npm_fetcher:open(Ctx),
    {ok, _Protocol} = gun:await_up(ConnPid),
    StreamRef = gun:get(ConnPid, Url, ReqHeaders),
    case gun:await(ConnPid, StreamRef) of
		{response, fin, Status, ResHeaders} ->
	  		cache(no_data,Url,ResHeaders,Status)
      	{response, nofin, Status, Headers} ->
	  		{ok, Body} = gun:await_body(ConnPid, StreamRef),
	  		Encoder = proplists:get_value(<<"content-encoding">>,Headers),
	  		Body2 = npm_fetcher:decode_body(Encoder, Body),
	  		if  
                Status  == 200 ->  cache(data,Url,ResHeaders,Body2);
                true ->  {data,Status,ResHeaders,Data}
            end
    end.

cache(data,Url,ResHeaders,Body)->
    Meta = jsx:decode(Body),
    CacheKey =
        case proplists:get_value(<<"name">>,Meta) of
            undefined -> undefined;
            Name ->
                ScopeName = npm_package:scope_name(Name),
                {atomic,ok} = ai_npm_package:add_package(ScopeName,Body),
                ScopeName
        end,
    case CacheKey of 
        undefined ->
            {data,ResHeaders,Body};
        _ ->
            ai_http_cache:cache(Url,CacheKey,ResHeaders),
            {data,ResHeaders,Body}
    end;
cache(no_data,Url,ResHeaders,Status)->
    if 
        Status == 304 ->
            ai_http_cache:cache(Url,ResHeaders),
            {no_data,Status,ResHeaders};
        true -> {no_data,Status,ResHeaders}
    end.