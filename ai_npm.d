src/ai_npm_app.erl:: include/npm_package.hrl include/npm_tarball.hrl; @touch $@
src/npm_package.erl:: include/npm_package.hrl; @touch $@
src/npm_package_api.erl:: include/npm_package.hrl include/req.hrl; @touch $@
src/npm_package_mnesia.erl:: include/npm_package.hrl; @touch $@

COMPILE_FIRST +=
