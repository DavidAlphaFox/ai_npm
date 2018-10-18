-record(package,{
                 key :: {binary(),binary()|undefined},
                 name :: binary(),
                 meta :: term()	
                }).
-record(package_cache,{
                       key :: {binary(),binary()|undefined},
                       cache_key :: {binary(),binary()|undefined},
                       date :: binary(),
                       etag :: binary(),
                       max_age :: integer(),
                       content_type :: binary(),
                       last_modified :: binary()
                      }).
