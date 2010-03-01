#!/usr/bin/env sh
cd `dirname $0`

# Check if Yaws exists..
if [ ! -d "../apps/yaws" ]; then
    echo "Could not find Yaws in '../apps/yaws'."
    echo "Run ../apps/get_yaws.sh to download from http://yaws.hyber.org/download"
    exit
fi

# Compile all required projects...
(cd ..; make compile)
(cd ../apps/yaws; [ -e "include.mk" ] || ./configure --disable-sendfile; cd src; make all)

# Link to support files...
echo Creating link to nitrogen support files...
rm -rf wwwroot/nitrogen
ln -s ../../apps/nitrogen/www wwwroot/nitrogen

# Start Nitrogen on Yaws...
echo Starting Nitrogen on Yaws...
erl \
    -name nitrogen_yaws@127.0.0.1 \
    -pa ./ebin ../apps/*/ebin ../apps/*/include \
    -s make all \
    -eval "application:start(mnesia)" \
    -eval "application:start(mprocreg)" \
    -eval "application:start(quickstart_yaws)"
