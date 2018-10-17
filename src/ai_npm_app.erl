-module(ai_npm_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	ai_npm_sup:start_link().

stop(_State) ->
	ok.
