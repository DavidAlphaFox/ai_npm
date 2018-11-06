-module(npm_tarball_storage).
-export([tmpfile/2,store/4,filename/1]).
-export([ensure_default_storage/0]).

-spec rename(TmpFile :: list() | binary(), FinalFile :: list() | binary()) -> {ok,list()|binary()} | {error, atom()}.
rename(TmpFile,FinalFile)->
    case filelib:ensure_dir(FinalFile) of
        ok ->
            case file:rename(TmpFile, FinalFile) of
                ok -> {ok,FinalFile};
                Error -> Error                    
            end;
        DirError -> DirError
    end.

-spec tmpfile(Scope :: binary(),Tarball :: binary() ) -> binary()| list().
tmpfile(Scope,Tarball)->
    Opts =    
        case Scope of
            undefined -> [{path,tmp_dir()}];
            _ -> [{prefix,erlang:binary_to_list(Scope)},{path,tmp_dir()}]
        end,
    ai_tmp:name(erlang:binary_to_list(Tarball),Opts).
-spec store(TmpFile :: list(),Scope :: binary(),Tarball :: binary()
    ,Digest :: list()) -> {ok,list()|binary()} | {error, atom()}.
store(TmpFile,Digest,Scope,Tarball)->
    Dir = case Scope of
            undefined -> filename:join([storage_dir(), Digest]);
            _ -> filename:join([storage_dir(), erlang:binary_to_list(Scope),Digest])
        end,
    FinalFile = filename:join([Dir,Tarball]),
    rename(TmpFile,FinalFile).

filename({Scope,Package,Version,Tarball})->
    case Scope of 
        undefined -> filename:join([Package,Version,Tarball]);
        _ -> filename:join([Scope,Package,Version,Tarball])
    end.

storage_dir()-> ai_file:priv_dir(ai_npm,"storage").
tmp_dir()-> ai_file:priv_dir(ai_npm,"tmp").
 
ensure_default_storage()->
    ok = ai_file:ensure_dir(storage_dir()),
    ok = ai_file:ensure_dir(tmp_dir()),
    ok.
