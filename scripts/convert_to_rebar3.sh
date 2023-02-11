#!/bin/sh

mkdir -pv rebar2_to_rebar3_trash
mkdir -pv priv
mv -v site/src .
mv -v site/include .
mv -v site/ebin rebar2_to_rebar3_trash
mv -v releases rebar2_to_rebar3_trash
mv -v fix-slim-release rebar2_to_rebar3_trash
mv -v lib rebar2_to_rebar3_trash
mv -v rebar rebar2_to_rebar3_trash
cp -v rebar.config rebar2_to_rebar3_trash/

mv -v site/static priv
mv -v site/templates priv
mv -v bin/nitrogen bin/nitrogen.old
mv -v bin/dev bin/dev.old
mv -v Makefile Makefile.old

ln -sv ../src site/src
ln -sv ../include site/include
ln -sv ../priv/static site/static
ln -sv ../priv/templates site/templates

BASEURL=https://raw.githubusercontent.com/nitrogen/nitrogen/rebar3

## Download new Makefile
curl $BASEURL/templates/common/Makefile -o Makefile

## download assemble_config.escript
curl $BASEURL/templates/common/etc/assemble_config.escript -o "etc/assemble_config.escript"
chmod 755 etc/assemble_config.escript

## download upgrade_release.sh
curl $BASEURL/templates/common/upgrade_release.sh -o "upgrade_release.sh"
chmod 755 upgrade_release.sh

## download copy_static.escript
curl $BASEURL/templates/common/copy_static.escript -o "copy_static.escript"
chmod 755 copy_static.escript

## download make_version_file.escript
curl $BASEURL/templates/common/make_version_file.escript -o "make_version_file.escript"
chmod 755 make_version_file.escript

## download new nitrogen/bin
curl $BASEURL/templates/common/bin/nitrogen -o "bin/nitrogen"
chmod 755 bin/nitrogen

## Get new plugins thing
curl $BASEURL/templates/common/do-plugins.escript -o "do-plugins.escript"
chmod 755 do-plugins.escript

## rewrite rebar.config based on current deps
curl $BASEURL/scripts/update_config_to_rebar3.escript -o "update_config_to_rebar3.escript"
chmod 755 update_config_to_rebar3.escript

## Get new plugins.config
curl $BASEURL/templates/common/plugins.config -o "plugins.config"

## Get erlang_ls.config for Erlang_LS
curl $BASEURL/templates/common/erlang_ls.config -o "erlang_ls.config"

./update_config_to_rebar3.escript

## Add additional new rebar3 rules

echo Done converting rebar2 nitrogen-based project to rebar3
