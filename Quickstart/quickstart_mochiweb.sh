#!/bin/sh
export NITROGEN_SRC=..
export MOCHIWEB_SRC=../http_servers/mochiweb
cd `dirname $0`

if [[ -d $MOCHIWEB_SRC ]]; then
	echo "Using Mochiweb in directory $MOCHIWEB_SRC."
else
	echo
	echo "Update \$MOCHIWEB_SRC to point to your Mochiweb directory."
	echo "Exiting..."
	echo 
	exit 1
fi

echo Creating link to Nitrogen support files...
rm -f content/wwwroot/nitrogen
ln -s ../../$NITROGEN_SRC/www content/wwwroot/nitrogen

echo Starting Nitrogen on Mochiweb...
exec erl \
	-name nitrogen@localhost \
	-pa $PWD/ebin $PWD/include \
	-pa $NITROGEN_SRC/ebin $NITROGEN_SRC/include \
	-pa $MOCHIWEB_SRC/ebin $MOCHIWEB_SRC/include \
	-sync_environment development \
	-s make all \
	-s mochiweb_helper


