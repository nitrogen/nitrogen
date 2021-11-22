#!/bin/sh

if [ -f "priv/nitrogen.plugin" ]; then
	echo "This plugin looks to already be configued for rebar3"
else
	if [ -f "nitrogen.plugin" ]; then
		mkdir -pv priv
		mv -v nitrogen.plugin priv/
		ln -sv priv/nitrogen.plugin
		echo "This plugin is rebar3-ready"
	else
		echo "This does not appear to be a Nitrogen plugin"
	fi
fi
		
