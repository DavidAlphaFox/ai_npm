-record(package,{
                 id :: {binary(),binary()| undefined}, %% {_id,_rev},_id is the name of package
                 meta :: term()	%% the json content of the package
                }).
-record(package_index,{name :: binary(),current :: binary()}).
-record(cache,{ key :: binary(), 
                cache_key :: term(),
                date :: binary(),
                etag :: binary(),
                max_age :: integer(),
                last_modified :: binary(),
                headers :: term()
              }).
