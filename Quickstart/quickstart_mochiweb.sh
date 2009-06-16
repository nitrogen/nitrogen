#!/bin/sh
export NITROGEN_HOME=..
export MOCHIWEB_HOME=../lib/mochiweb
export SIMPLEBRIDGE_HOME=../lib/simple_bridge
cd `dirname $0`

echo Creating link to Nitrogen support files...
rm -rf wwwroot/nitrogen
ln -s ../$NITROGEN_HOME/www wwwroot/nitrogen

echo Compile Nitrogen...
make -C $NITROGEN_HOME

echo Compile Mochiweb...
make -C $MOCHIWEB_HOME

echo Compile Simple Bridge...
make -C $SIMPLEBRIDGE_HOME

echo Starting Nitrogen on Mochiweb...
exec erl \
	-name nitrogen_mochiweb@127.0.0.1 \
	-pa $PWD/apps $PWD/ebin $PWD/include \
	-pa $NITROGEN_HOME/ebin $NITROGEN_HOME/include \
	-pa $SIMPLEBRIDGE_HOME/ebin $SIMPLEBRIDGE_HOME/include \
	-pa $MOCHIWEB_HOME/ebin $MOCHIWEB_HOME/include \
	-s make all \
	-eval "application:start(quickstart_mochiweb)"


