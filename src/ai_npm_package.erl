-module(ai_npm_package).
-include("ai_npm.hrl").

-export([find_by_name/1,find_by_name_version/3]).
-export([add_package/2,add_package_with_index/2]).

-spec find_by_name(Name :: binary()) -> {atomic,term()} | {aborted,term()}. 
find_by_name(Name)->
	F = fun() ->
		MatchHead = #package{id = '$1',_ = '_'},
		Guard = [{'==', {element,1,'$1'}, Name}],
		Result = ['$_'],
		mnesia:select(package, [{MatchHead, Guard, Result}])
	end,
	mnesia:transaction(F).


-spec find_by_name_version(Name :: binary(),Version :: binary(),atom()) -> {atomic,term()} | {aborted,term()}. 
find_by_name_version(Name,Version,Type)->
	case find_by_name(Name) of
			{atomic,[Item]} -> package_version(Item,Version,Type);
			{atomic,[]} -> not_found;
			Res -> Res
	end.
package_version(Item,Version,Type)->
		Json = jsx:decode(Item#package.meta),
		Versions = proplists:get_value(<<"versions">>,Json,[]),
		case proplists:get_value(Version,Versions,not_found) of
				not_found -> not_found;
				Meta -> 
						if binary == Type -> jsx:encode(Meta);
							 true -> Meta
						end
		end.
add_package_with_index(ID,Meta)->
		{Name,_} = ID,
		Package = #package{id = ID,meta = Meta},
		PackageIndex = #package_index{name = Name,current = ID},
		F = fun() ->
								ok = mnesia:write(Package),
								ok = mnesia:write(PackageIndex),
								ok
				end,
		mnesia:transaction(F).

add_package(ID,Meta)->
		Package = #package{id = ID,meta = Meta},
		F = fun() ->
								mnesia:write(Package)
				end,
		mnesia:transaction(F).
