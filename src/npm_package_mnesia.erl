-module(npm_package_mnesia).
-include("ai_npm.hrl").

-export([find/1,find/2]).
-export([add/1,add/2]).


-spec find(Name :: binary()) -> {atomic,term()} | {aborted,term()}. 
find(Name)->
    F = fun() ->
		MatchHead = #package{name = '$1',_ = '_'},
		Guard = [{'==', '$1', Name}],
		Result = ['$_'],
		mnesia:select(package, [{MatchHead, Guard, Result}])
	end,
	mnesia:transaction(F).

-spec find(Scope :: binary(),Name :: binary()) -> {atomic,term()} | {aborted,term()}. 
find(Scope,Name)->
    F = fun() ->
		MatchHead = #package{scope = '$1',name = '$2',_ = '_'},
		Guard = [{'==', '$1', Scope},{'==', '$2', Name}],
		Result = ['$_'],
		mnesia:select(package, [{MatchHead, Guard, Result}])
	end,
	mnesia:transaction(F).

-spec add(Scope :: binary(),Name :: binary(),Meta :: binary()) -> {atomic,ok} | {aborted,term()}.
add(Scope,Name,Meta)->
    add(Scope,Name,Meta,false).

-spec add(Scope :: binary(),Name :: binary(),Meta :: binary(),Type :: boolean()) -> {atomic,ok} | {aborted,term()}.
add(Scope,Name,Meta,Type)->
	Package = #package{socpe = Scope, name = Name,meta = Meta,private = Type},
	F = fun() -> mnesia:write(Package) end,
	mnesia:transaction(F).