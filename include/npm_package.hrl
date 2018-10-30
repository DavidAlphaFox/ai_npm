-define(NAME,<<"name">>).
-define(VERSIONS,<<"versions">>).
-define(DIST_TAGS,<<"dist-tags">>).
-define(ATTACHMENTS,<<"_attachments">>).

-record(package,{
                name :: tuple(),
                meta :: binary(),	%% the json content of the package
                digest :: binary(),
                private :: boolean()
                }).