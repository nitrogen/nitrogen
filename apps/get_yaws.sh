#!/usr/bin/env sh

DIR=$(cd ${0%/*} && pwd)
(cd $DIR; \
 curl --silent http://yaws.hyber.org/download/yaws-1.89.tar.gz | gunzip | tar -xv; \
 rm yaws; \ # Yaws .gz file creates a symbolic link that screws up packaging.
 cd yaws-1.89; \
 ./configure --disable-sendfile; \
 make)
