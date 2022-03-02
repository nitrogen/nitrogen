#!/bin/bash

set -e

APPNAME=nitrogen
CURRENT_VSN=`cat version.current`
PREVIOUS_VSN=`cat version.previous`
TARBALL=$APPNAME-$CURRENT_VSN.tar.gz

echo "Generating Release, Relup, and Release Tarball"
./rebar3 do release, appup generate --previous_version $PREVIOUS_VSN, relup -n $APPNAME -u $PREVIOUS_VSN -v $CURRENT_VSN, tar -o ./releases
mv releases/$APPNAME/$TARBALL _build/default/rel/$APPNAME/releases/

echo "Cleaning up releases directory (no longer needed)"
rm -fr releases

echo "Running the Upgrade: $PREVIOUS_VSN => $CURRENT_VSN"
cd _build/default/rel/$APPNAME
bin/$APPNAME upgrade $CURRENT_VSN


## for some reason, running upgrade removes  the $APPNAME.rel file from the release directory
## these next two lines restore them from the tarball before cleaning them up
echo "Extracting $APPNAME.rel from $TARBALL"
tar -xvzf releases/$TARBALL releases/$CURRENT_VSN/$APPNAME.rel
cp -v releases/$CURRENT_VSN/$APPNAME.rel releases/$APPNAME.rel

echo "Cleaning up $TARBALL"
rm -v releases/$TARBALL


