-module(ai_npm_mnesia_cache).
-include("ai_npm.hrl").

-export([try_hit_cache/1,hit_cache/1,add_to_cache/3,refresh_headers/2]).

transaction(F) ->
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, _Reason} -> []
    end.	
find_by_key(Table,Key) ->
    F = fun() ->
                mnesia:read(Table,Key)
        end,
    transaction(F).

write(Rec) ->
    F = fun() ->
                mnesia:write(Rec)
        end,
    case mnesia:transaction(F) of
        {atomic,ok} -> ok;
        Res -> Res
    end.
            

hit_cache(Key)->
    case find_by_key(cache,Key) of
        [] -> not_found;
        [C] -> C
    end.

cache_age(CachedData)->
    Now = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds(Now) - calendar:datetime_to_gregorian_seconds(CachedData).

cache_validate(#cache{date = Date,max_age = MaxAge} = C)->
    ParsedDate = ai_rfc822_date:parse(Date),
    CachedData = ai_rfc822_date:universal_time(ParsedDate),
    Age = cache_age(CachedData),
    if
        Age - MaxAge > 0 -> {expired,C};
        true -> {ok,C}
    end.
    

try_hit_cache(Key)->
    case hit_cache(Key) of
        not_found -> not_found;
        C -> cache_validate(C)
    end.

headers_to_fileds(Headers,Item)->    
    Need = fun({<<"date">>,V},Acc) ->Acc#cache{date = V};
              ({<<"etag">>,V},Acc) ->Acc#cache{etag = V};
              ({<<"last-modified">>,V},Acc) -> Acc#cache{last_modified = V};
              ({<<"cache-control">>,V},Acc) ->
                   Parts = binary:split(V,<<"=">>),
                   if 
                       erlang:length(Parts) == 2 ->
                           MaxAge = erlang:binary_to_integer(lists:nth(2,Parts)),
                           Acc#cache{max_age = MaxAge};
                       true -> Acc
                   end;
              (_,Acc) -> Acc 
              end,
    lists:foldl(Need,Item,Headers).
add_to_cache(Key,Headers,CacheKey)->
    Item = headers_to_fileds(Headers,#cache{key = Key,cache_key = CacheKey,headers = Headers}),
    write(Item).
refresh_headers(Key,Headers)->
    C = hit_cache(Key),
    Item = headers_to_fileds(Headers,C),
    write(Item).
