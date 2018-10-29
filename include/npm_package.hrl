-define(NAME,<<"name">>).
-define(VERSIONS,<<"versions">>).
-define(DIST_TAGS,<<"dist-tags">>).
-define(ATTACHMENTS,<<"_attachments">>).

-record(package,{
                scope :: binary(),
                name :: binary(), %%_id is the name of package
                meta :: binary(),	%% the json content of the package
                private :: boolean()
                }).