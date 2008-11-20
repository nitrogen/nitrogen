#!/bin/sh
export NITROGEN_SRC=..
cd `dirname $0`

echo Compiling...
make

echo Creating link to nitrogen support files...
rm -f wwwroot/nitrogen
ln -s ../$NITROGEN_SRC/www wwwroot/nitrogen

echo Starting Nitrogen on Inets...
exec erl \
	-pa $PWD/ebin $PWD/include \
	-pa $NITROGEN_SRC/ebin $NITROGEN_SRC/include \
	-s inets_helper

