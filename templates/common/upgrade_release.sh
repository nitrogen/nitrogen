#!/bin/sh

CURRENT_VSN=`cat version.current`
PREVIOUS_VSN=`cat version.previous`
./rebar3 do release, appup generate --previous_version $PREVIOUS_VSN, relup -n {{name}} -u $PREVIOUS_VSN -v $CURRENT_VSN, tar -o ./releases
mv releases/{{name}}/{{name}}-$CURRENT_VSN.tar.gz _build/default/rel/{{name}}/releases
cd _build/default/rel/{{name}}
bin/{{name}} upgrade $CURRENT_VSN
