-module(npm_package).
-include("npm_package.hrl").

-export([name/1,versions/1,dist_tags/1,attachments/1,scope/1]).
-export([version/2]).
-export([merge/2]).

-spec name(Json :: proplists:proplists()) -> binary() | undefined.
name(Json)-> proplists:get_value(?NAME,Json).
-spec scope(Json :: proplists:proplists()) -> binary() | undefined.
scope(Json) -> 
    case name(Json) of
        undefined -> undefined;
        _ ->
            case binary:split(Name) of 
                [Name] -> undefined;
                [Scope,_RName] -> Scope
            end 
    end. 
-spec versions(Json :: proplists:proplists()) -> binary() | undefined.
versions(Json) -> proplists:get_value(?VERSIONS,Json).
-spec dist_tags(Json :: proplists:proplists()) -> binary() | undefined.
dist_tags(Json) ->  proplists:get_value(?DIST_TAGS,Json).
-spec attachments(Json :: proplists:proplists()) -> binary() | undefined.
attachments(Json) -> proplists:get_value(?ATTACHMENTS,Json).
-spec version(Version::binary(),Json :: proplists:proplists()) -> proplists:proplists() | undefined.
version(Version,Json) ->
    case versions(Json) of
        undefined -> undefined;
        versions -> proplists:get_value(Version,versions)
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