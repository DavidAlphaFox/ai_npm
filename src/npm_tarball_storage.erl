-module(npm_tarball_storage).
-export([tmpfile/2,store/4]).
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

-spec tmpfile(Scope :: binary()|list(),Tarball :: binary() | list()) -> binary()| list().
tmpfile(Scope,Tarball)->
    Opts =    
        case Scope of
            undefined -> [{path,tmp_dir()}];
            _ -> [{prefix,Scope},{path,tmp_dir()}]
        end,
    ai_tmp:name(Tarball,Opts).
-spec store(TmpFile :: list() | binary(),Tar :: tuple()
    ,Digest :: list()|binary()) -> {ok,list()|binary()} | {error, atom()}.
store(TmpFile,Digest,Tar)->
    {Scope,_Package,_Version,Tarball} = Tar,
    Dir = case Scope of
            undefined -> filename:join([storage_dir(), Digest]);
            _ -> filename:join([storage_dir(), Scope,Digest])
        end,
    FinalFile = filename:join([Dir,Tarball]),
    rename(TmpFile,FinalFile).

storage_dir()-> ai_file:priv_dir(ai_npm,"storage").
tmp_dir()-> ai_file:priv_dir(ai_npm,"tmp").
 
ensure_default_storage()->
    ok = ai_file:ensure_dir(storage_dir()),
    ok = ai_file:ensure_dir(tmp_dir()),
    ok.
