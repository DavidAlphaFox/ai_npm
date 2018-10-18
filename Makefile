PROJECT = ai_npm
PROJECT_DESCRIPTION = A private npm registry and local cache
PROJECT_VERSION = 0.1.0
DEPS = cowboy gun ailib urilib jsx
LOCL_DEPS = crypto ans1 public_key ssl inets edoc compiler syntax_tools
dep_cowboy_commit = 2.5.0
dep_gun_commit = 1.3.0
dep_jsx_commit = v2.9.0
dep_ailib = git https://github.com/DavidAlphaFox/ailib.git master
dep_urilib = git https://github.com/gmr/urilib.git master
include erlang.mk
