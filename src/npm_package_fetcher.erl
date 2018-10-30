-module(npm_package_fetcher).
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
    ReqHeaders = npm_fetcher:headers(Ctx),
    CacheKey = proplists:get_value(cache_key,Ctx),
    {ok, ConnPid} = npm_fetcher:open(Ctx),
    {ok, _Protocol} = gun:await_up(ConnPid),
    StreamRef = gun:get(ConnPid, Url, ReqHeaders),
    case gun:await(ConnPid, StreamRef) of
		{response, fin, Status, ResHeaders} ->
	  		cache(no_data,Url,ResHeaders,Status,CacheKey);
      	{response, nofin, Status, ResHeaders} ->
	  		{ok, Body} = gun:await_body(ConnPid, StreamRef),
	  		Encoder = proplists:get_value(<<"content-encoding">>,ResHeaders),
	  		Body2 = npm_fetcher:decode_body(Encoder, Body),
	  		if  
                Status  == 200 ->  cache(data,Url,ResHeaders,Body2,CacheKey);
                true ->  {data,Status,ResHeaders,Body2}
            end
    end.

cache(data,Url,ResHeaders,Body,_CacheKey)->
    Meta = jsx:decode(Body),
    CacheKey =
        case proplists:get_value(<<"name">>,Meta) of
            undefined -> undefined;
            Name ->
                ScopeName = npm_package:scope_name(Name),
                {atomic,ok} = npm_package_mnesia:add(ScopeName,Body),
                ScopeName
        end,
    case CacheKey of 
        undefined ->
            {data,ResHeaders,Body};
        _ ->
            ai_http_cache:cache(Url,CacheKey,ResHeaders),
            {hit,CacheKey,ResHeaders}
    end;
cache(no_data,Url,ResHeaders,Status,CacheKey)->
    if 
        Status == 304 ->
            ai_http_cache:cache(Url,ResHeaders),
            {hit,CacheKey,Headers};
        true -> {no_data,Status,ResHeaders}
    end.