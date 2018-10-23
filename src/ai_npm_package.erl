-module(ai_npm_package).
-include("ai_npm.hrl").

-export([find_by_name/1,find_by_name_version/3]).
-export([add_package/2,add_private_package/2]).
-export([merge_package/1]).

-spec find_by_name(Name :: binary()) -> {atomic,term()} | {aborted,term()}. 
find_by_name(Name)->
	F = fun() ->
		MatchHead = #package{id = '$1',_ = '_'},
		Guard = [{'==', '$1', Name}],
		Result = ['$_'],
		mnesia:select(package, [{MatchHead, Guard, Result}])
	end,
	mnesia:transaction(F).

-spec find_by_name_version(Name :: binary(),Version :: binary(),atom()) -> {atomic,term()} | {aborted,term()}. 
find_by_name_version(Name,Version,Type)->
	case find_by_name(Name) of
			{atomic,[Item]} -> {atomic,package_version(Item,Version,Type)};
			{atomic,[]} -> {atomic,not_found};
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

add_private_package(Name,Meta)->
		Package = #package{id = Name,meta = Meta,private = true},
		F = fun() -> mnesia:write(Package) end,
		mnesia:transaction(F).

add_package(Name,Meta)->
		Package = #package{id = Name,meta = Meta,private = false},
		F = fun() -> mnesia:write(Package) end,
		mnesia:transaction(F).

merge_package(Json)->
    Name = proplists:get_value(<<"_id">>,Json),
    %%    {<<"dist-tags">>,[{<<"latest">>,<<"8.6.0">>}]},
    case find_by_name(Name) of 
        {atomic,[Item]} ->
            merge_package(Name,jsx:decode(Item#package.meta),Json);
        {atomic,[]} ->
            Json2 = proplists:delete(<<"_attachments">>,Json),
            add_private_package(Name,jsx:encode(Json2));
        Error -> Error
    end.
merge_package(Name,LJson,Json)->            
    DistTags = proplists:get_value(<<"dist-tags">>,Json),
    Versions = proplists:get_value(<<"versions">>,Json),
    LVersions = proplists:get_value(<<"versions">>,LJson),
    MeregedVersions = lists:foldl(fun({Version,_Content} = I,Acc)->
                        NewAcc = proplists:delete(Version,Acc),
                        [I | NewAcc]
                end,LVersions,Versions),
    LJson2 = proplists:delete(<<"dist-tags">>,LJson),
    LJson3 = proplists:delete(<<"versions">>,LJson2),   
    NewPackage = jsx:encode([{<<"dist-tags">>,DistTags},{<<"versions">>,MeregedVersions}] ++ LJson3),
    add_private_package(Name,NewPackage).


