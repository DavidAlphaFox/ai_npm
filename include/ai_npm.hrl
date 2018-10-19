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
                       last_modified :: binary(),
                       headers :: term()
                      }).
