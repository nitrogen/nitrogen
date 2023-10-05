#!/bin/bash

set -e

APPNAME=nitrogen
CURRENT_VSN=`cat version.current`
PREVIOUS_VSN=`cat version.previous`
TARBALL=$APPNAME-$CURRENT_VSN.tar.gz

echo "********************************************************************************"
echo "Generating Release..."
echo "********************************************************************************"
./rebar3 release

echo "********************************************************************************"
echo "Generating Appups..."
echo "********************************************************************************"
./rebar3 appup generate --previous_version $PREVIOUS_VSN --current _build/default/rel/$APPNAME

echo "********************************************************************************"
echo "Generating Relups..."
echo "********************************************************************************"
./rebar3 relup -n $APPNAME -u $PREVIOUS_VSN -v $CURRENT_VSN

echo "********************************************************************************"
echo "Generating Tarball"
echo "********************************************************************************"
./rebar3 tar -o ./releases

echo "********************************************************************************"
echo "Moving the Tarball _build/default/rel/$APPNAME/releases"
echo "********************************************************************************"
mv releases/$APPNAME/$TARBALL _build/default/rel/$APPNAME/releases/

echo "********************************************************************************"
echo "Cleaning up releases directory"
echo "********************************************************************************"
rm -fr releases

echo "********************************************************************************"
echo "Performing the Upgrade: $PREVIOUS_VSN => $CURRENT_VSN"
echo "********************************************************************************"
cd _build/default/rel/$APPNAME
bin/$APPNAME upgrade $CURRENT_VSN

## for some reason, running upgrade removes the $APPNAME.rel file from the release directory
## these next two lines restore them from the tarball before cleaning them up
echo "********************************************************************************"
echo "Extracting $APPNAME.rel from $TARBALL"
echo "********************************************************************************"
tar -xvzf releases/$TARBALL releases/$CURRENT_VSN/$APPNAME.rel
cp -v releases/$CURRENT_VSN/$APPNAME.rel releases/$APPNAME.rel

echo "********************************************************************************"
echo "Cleaning up $TARBALL"
echo "********************************************************************************"
rm -v releases/$TARBALL


