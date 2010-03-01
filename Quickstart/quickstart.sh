#!/usr/bin/env sh
cd `dirname $0`

# Compile all required projects...
(cd ..; make compile)

# Link to support files...
echo Creating link to nitrogen support files...
rm -rf wwwroot/nitrogen
ln -s ../../apps/nitrogen/www wwwroot/nitrogen

# Start Nitrogen on Inets...
echo Starting Nitrogen on Inets...
erl \
    -name nitrogen@127.0.0.1 \
    -pa ./ebin ../apps/*/ebin ../apps/*/include \
    -s make all \
    -eval "application:start(mnesia)" \
    -eval "application:start(mprocreg)" \
    -eval "application:start(quickstart_inets)"
