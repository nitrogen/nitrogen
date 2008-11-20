#!/bin/sh
export NITROGEN_SRC=..
cd `dirname $0`

echo Compiling...
make

echo Creating link to nitrogen support files...
rm -f content/wwwroot/nitrogen
ln -s ../../$NITROGEN_SRC/www content/wwwroot/nitrogen

echo Starting Nitrogen on Inets...
exec erl \
	-name nitrogen@localhost \
	-pa $PWD/ebin $PWD/include \
	-pa $NITROGEN_SRC/ebin $NITROGEN_SRC/include \
	-sync_environment development \
	-s inets_helper


