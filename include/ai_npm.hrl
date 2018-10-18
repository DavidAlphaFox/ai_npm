-record(package,{
		key :: {list(),list()|undefined},
		name :: list(),
		meta :: term()	
	}).
-record(package_cache,{
	key :: {list(),list()|undefined},
	date :: list(),
	etag :: list(),
	max_age :: integer(),
	content_type :: list(),
	last_modified :: list()
	}).
