#!/usr/bin/env sh

BASE=`dirname $0`
curl --silent http://yaws.hyber.org/download/yaws-1.87.tar.gz | gunzip | tar -xv -C $BASE