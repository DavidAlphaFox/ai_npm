-module(ai_npm_storage).
-export([rename/2]).
-export([ensure_default_storage/0]).

-spec rename(TmpFile :: list(), FinalFile :: list()) -> {ok,list()} | {error, atom()}.
rename(TmpFile,FinalFile)->
    case filelib:ensure_dir(FinalFile) of
        ok ->
            case file:rename(TmpFile, FinalFile) of
                ok -> {ok,FinalFile};
                Error -> Error                    
            end;
        DirError -> DirError
    end.
ensure_default_storage()->
    ok = ai_file:create_priv_dir(ai_npm,"storage"),
    ok = ai_file:create_priv_dir(ai_npm,"tmp"),
    ok.
