-module(ai_npm_mnesia).
-include("ai_npm.hrl").

-export([try_hint_cache/2,add_to_cache/4]).
-export([retrive_data/1,add_data/3]).

transaction(F) ->
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, _Reason} -> []
    end.	
find_by_key(Key) ->
    F = fun() ->
                mnesia:read(Key)
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
            

hit_cache(Name,Version)->
    Key = {Name,Version},
    case find_by_key(Key) of
        [] -> not_fond;
        [C] -> C
    end.

cache_age(CachedData)->
    Now = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds(Now) - calendar:datetime_to_gregorian_seconds(CachedData).

cache_validate(#package_cache{date = Date,max_age = MaxAge} = C)->
    ParsedDate = ai_rfc822_date:parse(Date),
    CachedData = ai_rfc822_date:universal_time(ParsedDate),
    Age = cache_age(CachedData),
    if
        Age - MaxAge > 0 -> {expired,C};
        true -> {ok,C}
    end.
    

try_hint_cache(Name,Version)->
    case hit_cache(Name,Version) of
        not_fond -> not_fond;
        C -> cache_validate(C)
    end.

headers_to_fileds(Headers,Item)->    
    Need = fun({<<"date">>,V},Acc) ->Acc#package_cache{date = V};
              ({<<"etag">>,V},Acc) ->Acc#package_cache{etag = V};
              ({<<"content-type">>,V},Acc) -> Acc#package_cache{content_type = V};
              ({<<"last-modified">>,V},Acc) -> Acc#package_cache{last_modified = V};
              ({<<"cache-control">>,V},Acc) ->
                   Parts = binary:split(V,<<"=">>),
                   if 
                       erlang:length(Parts) == 2 ->
                           MaxAge = erlang:binary_to_integer(lists:nth(2,Parts)),
                           Acc#package_cache{max_age = MaxAge};
                       true -> Acc
                   end;
              (_,Acc) -> Acc 
              end,
    lists:foldl(Need,Item,Headers).
add_to_cache(Name,Version,Headers,CacheKey)->
    Key = {Name,Version},
    Item = headers_to_fileds(Headers,#package_cache{key = Key,cache_key = CacheKey}),
    write(Item).
retrive_data(CacheKey)->
    [C] = find_by_key(CacheKey),
    C.
add_data(CacheKey,Name,Meta)->
    Item = #package{ key = CacheKey,name = Name,meta = Meta},
    write(Item).
