-record(tarball,{
                scope :: binary(),
                name :: binary(), %%_id is the name of package
                version :: binary(),	%% the json content of the package
                private :: boolean(),
                path :: binary()
                }).