#!/bin/bash

if git rev-parse --git-dir >/dev/null 2>&1; then
	echo "yes"
else
	echo "no"
fi
