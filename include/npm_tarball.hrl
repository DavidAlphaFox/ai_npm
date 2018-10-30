-record(tarball,{
                name :: tuple(), %%_id is the name of package
                version :: binary(),	%% the json content of the package
                private :: boolean(),
                path :: binary()
                }).