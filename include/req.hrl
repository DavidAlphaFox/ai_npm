-define(PUT,<<"PUT">>).
-define(GET,<<"GET">>).
-define(PACKAGE_HEADERS,[
                           {<<"content-type">>, <<"application/json">>},
                           {<<"content-encoding">>,<<"identity">>}
                        ]).
-define(DEFAULT_HEADERS,[
                        {<<"content-type">>, <<"application/octet-stream">>}
                    ]).