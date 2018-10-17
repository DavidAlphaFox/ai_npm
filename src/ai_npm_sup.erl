-module(ai_npm_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-define(DEFAULT_NPM_CACHE_SIZE, "2GB").
-define(DEFAULT_NPM_CACHE_TTL, 3600000).
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    CacheOpts = [
                 {name, ai_npm_ets_cache},
                 {ttl, ?DEFAULT_NPM_CACHE_TTL},
                 {size, ?DEFAULT_NPM_CACHE_SIZE}
                ],
    Procs = [{
               ai_npm_ets_cache,
               {ai_npm_ets_cache, start_link, [CacheOpts]},
               permanent, 2000, worker, dynamic
              }],
    {ok, {{one_for_one, 1, 5}, Procs}}.
