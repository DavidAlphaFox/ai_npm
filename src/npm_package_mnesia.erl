-module(npm_package_mnesia).
-include("npm_package.hrl").

-export([find/1]).
-export([add/2,add/3]).


-spec find(Name :: tuple()) -> {atomic,term()} | {aborted,term()}. 
find(Name)->
    F = fun() ->
		MatchHead = #package{name = '$1',_ = '_'},
		Guard = [{'==', '$1', {Name}}],
		Result = ['$_'],
		mnesia:select(package, [{MatchHead, Guard, Result}])
	end,
	mnesia:transaction(F).

-spec add(Name :: tuple(),Meta :: binary()) -> {atomic,ok} | {aborted,term()}.
add(Name,Meta)->
    add(Name,Meta,false).

-spec add(Name :: tuple(),Meta :: binary(),Type :: boolean()) -> {atomic,ok} | {aborted,term()}.
add(Name,Meta,Type)->
	Digest = ai_strings:sha256_string(Meta,lower),
	Package = #package{name = Name,meta = Meta,digest = Digest,private = Type},
	F = fun() -> mnesia:write(Package) end,
	mnesia:transaction(F).