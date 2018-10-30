-module(npm_tarball_mnesia).
-include("npm_tarball.hrl").

-export([find/1,find/2,find/3]).
-export([add/3,add/4]).


-spec find(Name :: tuple()) -> {atomic,term()} | {aborted,term()}. 
find(Name)->
    F = fun() ->
		MatchHead = #tarball{name = '$1',_ = '_'},
		Guard = [{'==', '$1', {Name}}],
		Result = ['$_'],
		mnesia:select(tarball, [{MatchHead, Guard, Result}])
	end,
	mnesia:transaction(F).
-spec find(Name :: tuple(),Version :: binary()) -> {atomic,term()} | {aborted,term()}. 
find(Name,Version)->
    F = fun() ->
		MatchHead = #tarball{name = '$1',version = '$2',_ = '_'},
		Guard = [{'==', '$1', {Name}},{'==','$2',Version}],
		Result = ['$_'],
		mnesia:select(tarball, [{MatchHead, Guard, Result}])
	end,
	mnesia:transaction(F).
-spec find(Name :: tuple(),
	Version :: binary(),Type :: binary()) -> {atomic,term()} | {aborted,term()}. 
find(Name,Version,Type)->
    F = fun() ->
		MatchHead = #tarball{name = '$1',version = '$2',private = '$3',_ = '_'},
		Guard = [{'==', '$1', {Name}},{'==','$2',Version},{'==','$3',Type}],
		Result = ['$_'],
		mnesia:select(tarball, [{MatchHead, Guard, Result}])
	end,
	mnesia:transaction(F).
-spec add(Name :: tuple(),
    Version:: binary(), Path :: binary()) -> {atomic,ok} | {aborted,term()}.
add(Name,Version,Path)->
    add(Name,Version,Path,false).

-spec add(Name :: tuple(),
    Version:: binary(), Path :: binary(),Type :: boolean()) -> {atomic,ok} | {aborted,term()}.
add(Name,Version,Path,Type)->
	Package = #tarball{name = Name,version = Version,path = Path,private = Type},
	F = fun() -> mnesia:write(Package) end,
	mnesia:transaction(F).