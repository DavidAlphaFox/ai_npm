-define(NAME,<<"name">>).
-define(VERSIONS,<<"versions">>).
-define(DIST_TAGS,<<"dist-tags">>).
-define(ATTACHMENTS,<<"_attachments">>).
-define(DATA,<<"data">>).
-define(DIST,<<"dist">>).
-define(TARBALL,<<"tarball">>).
-record(package,{
    name :: tuple(),
    meta :: binary()
    }).
-record(private_package,{
    name :: tuple(),
    meta :: binary()
}).