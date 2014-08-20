#!/usr/bin/env bash

## This is just to get around the fact that OSX doesn't ship with a decent
## version of readlink that supports -f

realpath() {
	[[ $1 = /* ]] && echo "$1" || echo "$PWD/${1#./}"
}

realpath "$1"
