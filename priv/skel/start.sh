#!/bin/sh
cd `dirname $0`

echo Starting Nitrogen.
erl \
	-name nitrogen@localhost \
	-pa ./ebin -pa ./include \
	-env sync_environment development \
	-s make all \
	-eval "application:start(SKEL)"