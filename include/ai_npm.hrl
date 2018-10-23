-record(package,{
                 id :: binary(), %%_id is the name of package
                 meta :: binary(),	%% the json content of the package
                 private :: boolean()
                }).
-record(cache,{ key :: binary(), 
                cache_key :: term(),
                date :: binary(),
                etag :: binary(),
                max_age :: integer(),
                last_modified :: binary(),
                headers :: term()
              }).
