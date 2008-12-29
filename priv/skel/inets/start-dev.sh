#!/bin/sh
export NITROGEN_SRC=..
cd `dirname $0`

echo Starting Nitrogen on Inets...
erl \
	-name nitrogen@localhost \
	-pa $PWD/ebin $PWD/include \
	-pa $NITROGEN_SRC/ebin $NITROGEN_SRC/include \
	-sync_environment development \
	-s make all \
	-s inets_helper


