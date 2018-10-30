-define(PUT,<<"PUT">>).
-define(GET,<<"GET">>).
-define(PACKAGE_HEADERS,[
                           {<<"content-type">>, <<"application/json">>}
                        ]).
-define(DEFAULT_HEADERS,[
                        {<<"content-type">>, <<"application/octet-stream">>}
                    ]).
-define(TARBALL_HEADERS,[
        {<<"content-type">>, <<"application/octet-stream">>}
    ]).