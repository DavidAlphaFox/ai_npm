-module(npm_tarball_mnesia).
-include("npm_tarball.hrl").

-export([find/1,find/2,find/3]).
-export([add/4,add/5]).


-spec find(Name :: binary()) -> {atomic,term()} | {aborted,term()}. 
find(Name)->
    F = fun() ->
		MatchHead = #tarball{name = '$1',_ = '_'},
		Guard = [{'==', '$1', Name}],
		Result = ['$_'],
		mnesia:select(tarball, [{MatchHead, Guard, Result}])
	end,
	mnesia:transaction(F).

-spec find(Scope :: binary(),Name :: binary()) -> {atomic,term()} | {aborted,term()}. 
find(Scope,Name)->
    F = fun() ->
		MatchHead = #tarball{scope = '$1',name = '$2',_ = '_'},
		Guard = [{'==', '$1', Scope},{'==', '$2', Name}],
		Result = ['$_'],
		mnesia:select(tarball, [{MatchHead, Guard, Result}])
	end,
	mnesia:transaction(F).
-spec find(Scope :: binary(),Name :: binary(),Version :: binary()) -> {atomic,term()} | {aborted,term()}. 
find(Scope,Name,Version)->
    F = fun() ->
		MatchHead = #tarball{scope = '$1',name = '$2',version = '$3',_ = '_'},
		Guard = [{'==', '$1', Scope},{'==', '$2', Name},{'==','$3',Version}],
		Result = ['$_'],
		mnesia:select(tarball, [{MatchHead, Guard, Result}])
	end,
	mnesia:transaction(F).
-spec add(Scope :: binary(),Name :: binary(),
    Version:: binary(), Path :: binary()) -> {atomic,ok} | {aborted,term()}.
add(Scope,Name,Version,Path)->
    add(Scope,Name,Version,Path,false).

-spec add(Scope :: binary(),Name :: binary(),
    Version:: binary(), Path :: binary(),Type :: boolean()) -> {atomic,ok} | {aborted,term()}.
add(Scope,Name,Version,Path,Type)->
	Package = #tarball{socpe = Scope, name = Name,version = Version,path = Path,private = Type},
	F = fun() -> mnesia:write(Package) end,
	mnesia:transaction(F).