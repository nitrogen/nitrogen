#!/bin/sh
export NITROGEN_SRC=..
export YAWS_SRC=../http_servers/yaws
cd `dirname $0`

if [[ -d $YAWS_SRC ]]; then
	echo "Using Yaws in directory $YAWS_SRC."
else
	echo
	echo "Update \$YAWS_SRC to point to your Yaws directory."
	echo "Exiting..."
	echo 
	exit 1
fi

echo Creating link to nitrogen support files...
rm -rf wwwroot/nitrogen
ln -s ../$NITROGEN_SRC/www wwwroot/nitrogen

echo Starting Nitrogen on Yaws...
exec erl \
	-name nitrogen@127.0.0.1 \
	-pa $PWD/apps $PWD/ebin $PWD/include \
	-pa $NITROGEN_SRC/ebin $NITROGEN_SRC/include \
	-pa $YAWS_SRC/ebin $YAWS_SRC/include \
	-s make all \
	-eval "application:start(quickstart_yaws)"
