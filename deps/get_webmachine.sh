#!/usr/bin/env sh

DIR=$(cd ${0%/*} && pwd)
(cd $DIR; \
 git clone git@github.com:basho/webmachine.git webmachine;
 cd webmachine; \
 git checkout webmachine-1.7.3; \
 make)
