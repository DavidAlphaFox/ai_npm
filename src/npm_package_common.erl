-module(npm_package_common).
-include("npm_package.hrl").

-export([name/1,versions/1,dist_tags/1,attachments/1,scope_name/1]).
-export([version_info/2,version/2]).
-export([merge/2]).

-spec name(Json :: proplists:proplists()) -> binary() | undefined.
name(Json)-> proplists:get_value(?NAME,Json).

-spec versions(Json :: proplists:proplists()) -> binary() | undefined.
versions(Json) -> proplists:get_value(?VERSIONS,Json).
-spec dist_tags(Json :: proplists:proplists()) -> binary() | undefined.
dist_tags(Json) ->  proplists:get_value(?DIST_TAGS,Json).
-spec attachments(Json :: proplists:proplists()) -> binary() | undefined.
attachments(Json) -> proplists:get_value(?ATTACHMENTS,Json).
-spec version_info(Version::binary(),Json :: proplists:proplists()) -> proplists:proplists() | undefined.
version_info(Version,Json) ->
    case versions(Json) of
        undefined -> undefined;
        Versions -> proplists:get_value(Version,Versions)
    end.
-spec version(Package :: binary(),Tarball :: binary()) -> binary().
version(Package,Tarball)->
    Tail = ai_string:prefix(Tarball,<<Package/binary,"-">>),
    Suffix = ai_string:find(Tail,".",trailing),
    SLen = erlang:byte_size(Suffix),
    TLen = erlang:byte_size(Tail),
    ai_string:slice(Tail,0,TLen - SLen).
-spec scope_name(Name :: binary()) -> binary() | undefined.
scope_name(Name) -> 
    case binary:split(Name,<<"/">>) of 
        [Name] -> {undefined,Name};
        [Scope,RName] -> {Scope,RName}
    end.
-spec merge(ORecord :: proplists:proplists(),NRecord :: proplists:proplists()) -> proplists:proplists().
merge(ORecord,NRecord)->
    DistTags = dist_tags(NRecord),
    OVersions = versions(ORecord),
    NVersions = versions(NRecord),
    MeregedVersions = lists:foldl(fun({Version,_C} = I,Acc)->
        NewAcc = proplists:delete(Version,Acc),
        [I | NewAcc]
    end,OVersions,NVersions), 
    CRecord = lists:foldl(fun(Key,Acc)-> 
            proplists:delete(Key,Acc)
        end,ORecord,[?VERSIONS,?DIST_TAGS]),
    [{?DIST_TAGS,DistTags},{?VERSIONS,MeregedVersions}] ++ CRecord.