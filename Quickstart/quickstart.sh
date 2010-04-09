#!/usr/bin/env sh
cd `dirname $0`

# Compile all required projects...
(cd ..; make compile)

# Link to nitrogen files...
(cd static; rm nitrogen; ln -s ../../apps/nitrogen/www nitrogen)

# Make all files.
make

# Start Nitrogen on Inets...
echo "Starting Nitrogen on Inets (http://localhost:8000)..."
erl \
    -name nitrogen@127.0.0.1 \
    -pa ./ebin ../apps/*/ebin ../apps/*/include  \
    -env ERL_FULLSWEEP_AFTER 10 \
    -eval "application:start(mnesia)" \
    -eval "application:start(mprocreg)" \
    -eval "application:start(quickstart)"
