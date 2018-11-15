-module(npm_package_mnesia).
-include("npm_package.hrl").

-export([find/1,find_private/1]).
-export([add/2,add_private/2]).


-spec find(Name :: tuple()) -> {atomic,term()} | {aborted,term()}. 
find(Name)->
    F = fun() ->
		MatchHead = #package{name = '$1',_ = '_'},
		Guard = [{'==', '$1', {Name}}],
		Result = ['$_'],
		mnesia:select(package, [{MatchHead, Guard, Result}])
	end,
	mnesia:transaction(F).
-spec find_private(Name :: tuple()) -> {atomic,term()} | {aborted,term()}. 
find_private(Name) ->
	F = fun()->
			MatchHead = #package{name = '$1',_ = '_'},
			Guard = [{'==', '$1', {Name}}],
			Result = ['$_'],
			mnesia:select(private_package, [{MatchHead, Guard, Result}])
		end,
	mnesia:transaction(F).
-spec add(Name :: tuple(),Meta :: binary()) -> {atomic,ok} | {aborted,term()}.
add(Name,Meta)->
	Package = #package{name = Name,meta = Meta},
	F = fun() -> mnesia:write(Package) end,
	mnesia:transaction(F).

-spec add_private(Name :: tuple(),Meta :: binary()) -> {atomic,ok} | {aborted,term()}.
add_private(Name,Meta)->
	Package = #private_package{name = Name,meta = Meta},
	F = fun() -> mnesia:write(Package) end,
	mnesia:transaction(F).
