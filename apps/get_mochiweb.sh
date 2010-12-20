#!/usr/bin/env sh

DIR=$(cd ${0%/*} && pwd)
(cd $DIR; \
 git clone git://github.com/mochi/mochiweb.git mochiweb; \
 cd mochiweb; \ 
 git checkout 1.5.0; \
 make)
