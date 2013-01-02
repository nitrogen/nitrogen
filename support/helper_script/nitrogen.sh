#!/bin/bash
# Nitrogen initialization script for *nix OSes
#
# Public Domain
#
# Then you can make nitrogen and dev calls from within any directory in a
# Nitrogen 2.0 installation.
#
# For example, say you're in ./site/src/libs, and you want to create a new page
# called "login", you would type "dev page login", while normally, you would
# have to type "../../../bin/dev page login"
#
# This is especially convenient if you have more than one Nitrogen installation.

function find_nitro() {
	if [ -x "bin/nitrogen" ] && [ -x "bin/dev" ] ; then
		./bin/$1 $2 $3
	else
		if [ `pwd` == "/" ]; then
			echo Not in a Nitrogen installation. This can only be executed from inside a Nitrogen installation
		else
			cd ..
			find_nitro $1 $2 $3
		fi
	fi
}

if [ `basename $0` == "nitrogen" ]; then
	find_nitro `basename $0` $1
else
	if [ `basename $0` == "dev" ]; then
		find_nitro `basename $0` $1 $2
	else
		echo "Usage: [nitrogen|dev] arguments"
	fi
fi
