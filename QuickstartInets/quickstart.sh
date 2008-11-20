#!/bin/sh
export NITROGEN_SRC=..
cd `dirname $0`
exec erl \
	-pa $PWD/ebin $PWD/include \
	-pa $NITROGEN_SRC/ebin $NITROGEN_SRC/include \
	-s inets_helper

