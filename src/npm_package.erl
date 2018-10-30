-module(npm_package).
-include("npm_package.hrl").

-export([name/1,versions/1,dist_tags/1,attachments/1,scope_name/1]).
-export([version_info/2,version/2]).
-export([merge/2]).

-spec name(Json :: proplists:proplists()) -> binary() | undefined.
name(Json)-> proplists:get_value(?NAME,Json).
-spec scope(Name :: binary()) -> binary() | undefined.
scope_name(Name) -> 
    case binary:split(Name,<<"/">>) of 
        [Name] -> {undefined,Name};
        [Scope,RName] -> {Scope,RName}
    end.
-spec versions(Json :: proplists:proplists()) -> binary() | undefined.
versions(Json) -> proplists:get_value(?VERSIONS,Json).
-spec dist_tags(Json :: proplists:proplists()) -> binary() | undefined.
dist_tags(Json) ->  proplists:get_value(?DIST_TAGS,Json).
-spec attachments(Json :: proplists:proplists()) -> binary() | undefined.
attachments(Json) -> proplists:get_value(?ATTACHMENTS,Json).
-spec version(Version::binary(),Json :: proplists:proplists()) -> proplists:proplists() | undefined.
version_info(Version,Json) ->
    case versions(Json) of
        undefined -> undefined;
        Versions -> proplists:get_value(Version,Versions)
    end.

version(Package,Tarball)->
    Tail = string:prefix(Tarball,<<Package,"-">>),
    Suffix = string:find(Tail,".",trailing),
    SLen = erlang:byte_size(Suffix),
    TLen = erlang:byte_size(Tail),
    string:slice(Tail,0,TLen - SLen).

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