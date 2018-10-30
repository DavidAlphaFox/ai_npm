-module(npm_cache).
-export([run_cache/2]).
run_cache(Url,MFA)->
    Hit = ai_http_cache:validate_hit(Url),
    ai_function:run_mfa(MFA,[Hit]).