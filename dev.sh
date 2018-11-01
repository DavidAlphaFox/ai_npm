#!/bin/sh
cd `dirname $0`
export ERL_MAX_PORTS=2048
exec erl +K true -pa $(pwd)/ebin $(find $(pwd)/deps -type d -name ebin | xargs) -s ai_npm
