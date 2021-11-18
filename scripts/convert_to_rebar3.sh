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

## Download new Makefile

curl https://raw.githubusercontent.com/nitrogen/nitrogen/rebar3/templates/common/Makefile -o Makefile

## download rebar3

curl https://raw.githubusercontent.com/nitrogen/nitrogen/rebar3/rebar3 -o "rebar3"
chmod 755 rebar3

## download assemble_config.escript

curl https://raw.githubusercontent.com/nitrogen/nitrogen/rebar3/templates/common/etc/assemble_config.escript -o "etc/assemble_config.escript"
chmod 755 etc/assemble_config.escript

## download new nitrogen/bin

curl https://raw.githubusercontent.com/nitrogen/nitrogen/rebar3/templates/common/bin/nitrogen -o "bin/nitrogen"
chmod 755 bin/nitrogen

## Get new plugins thing
curl https://raw.githubusercontent.com/nitrogen/nitrogen/rebar3/templates/common/do-plugins.escript -o "do-plugins.escript"
chmod 755 do-plugins.escript

## rewrite rebar.config based on current deps
curl https://raw.githubusercontent.com/nitrogen/nitrogen/rebar3/scripts/update_config_to_rebar3.escript -o "update_config_to_rebar3.escript"
chmod 755 update_config_to_rebar3.escript

./update_config_to_rebar3.escript

## Add additional new rebar3 rules

echo Done converting rebar2 nitrogen-based project to rebar3
