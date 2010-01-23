#!/usr/bin/env sh
cd `dirname $0`

# Check if Mochiweb exists..
if [ ! -d "../apps/mochiweb" ]; then
    echo "Could not find Mochiweb in '../apps/mochiweb'."
    echo "Run ../apps/get_mochiweb.sh to download from http://mochiweb.googlecode.com"
    exit
fi

# Compile all required projects...
(cd ..; make compile)
(cd ../apps/mochiweb; make all)

# Link to support files...
echo Creating link to nitrogen support files...
rm -rf wwwroot/nitrogen
ln -s ../../apps/nitrogen/www wwwroot/nitrogen

# Start Nitrogen on Mochiweb...
echo Starting Nitrogen on Mochiweb...
erl \
    -name nitrogen_mochiweb@127.0.0.1 \
    -pa ./ebin ../apps/*/ebin ../apps/*/include \
    -s make all \
    -eval "application:start(quickstart_mochiweb)"
