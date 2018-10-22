-module(ai_npm_package).
-include("ai_npm.hrl").

-export([find_by_name/1,find_by_name_version/3,find_by_id/1,find_by_id_version/3]).
-export([add_package/2,add_private_package/3]).

-spec find_by_name(Name :: binary()) -> {atomic,term()} | {aborted,term()}. 
find_by_name(Name)->
	F = fun() ->
		MatchHead = #package{id = '$1',_ = '_'},
		Guard = [{'==', {element,1,'$1'}, Name}],
		Result = ['$_'],
		mnesia:select(package, [{MatchHead, Guard, Result}])
	end,
	mnesia:transaction(F).

-spec find_by_id(ID :: {binary(),binary()}) -> {atomic,term()} | {aborted,term()}. 
find_by_id(ID)->
    F = fun() ->
                MatchHead = #package{id = '$1',_ = '_'},
                Guard = [{'==', '$1', {ID}}],
                Result = ['$_'],
                mnesia:select(package, [{MatchHead, Guard, Result}])
        end,
    mnesia:transaction(F).
-spec find_by_id_version(Name :: binary(),Version :: binary(),atom()) -> {atomic,term()} | {aborted,term()}. 
find_by_id_version(ID,Version,Type)->
    case find_by_id(ID) of
        {atomic,[Item]} -> {ok,package_version(Item,Version,Type)};
        {atomic,[]} -> not_found;
        Res -> Res
    end.
        
-spec find_by_name_version(Name :: binary(),Version :: binary(),atom()) -> {atomic,term()} | {aborted,term()}. 
find_by_name_version(Name,Version,Type)->
	case find_by_name(Name) of
			{atomic,[Item]} -> {ok,package_version(Item,Version,Type)};
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
add_private_package(Name,Rev,Meta)->
    ID = {Name,Rev},
		Package = #package{id = ID,meta = Meta},
		PackagePrivate = #package_private{name = Name,current = ID},
		F = fun() ->
								ok = mnesia:write(Package),
								ok = mnesia:write(PackagePrivate),
								ok
				end,
		mnesia:transaction(F).

add_package(ID,Meta)->
		Package = #package{id = ID,meta = Meta},
		F = fun() ->
								mnesia:write(Package)
				end,
		mnesia:transaction(F).
