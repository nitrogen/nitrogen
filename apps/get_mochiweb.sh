#!/usr/bin/env sh

DIR=$(cd ${0%/*} && pwd)
(cd $DIR; \
 svn checkout http://mochiweb.googlecode.com/svn/trunk@170 mochiweb; \
 cd mochiweb;
 make all)
