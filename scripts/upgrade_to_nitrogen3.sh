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
OPTS=-H'Cache-Control:no-cache,no-store'

## Download new Makefile
echo "Downloading $BASEURL/templates/common/Makefile"
curl $OPTS $BASEURL/templates/common/Makefile -o Makefile

## Replace {{name}} with nitrogen in the Makefile
echo "Setting the app name to 'nitrogen' in the Makefile"
sed -i 's/{{name}}/nitrogen/g' Makefile

## download assemble_config.escript
echo "Downloading $BASEURL/templates/common/etc/assemble_config.escript"
curl $OPTS $BASEURL/templates/common/etc/assemble_config.escript -o "etc/assemble_config.escript"
chmod 755 etc/assemble_config.escript

## download upgrade_release.sh
echo "Downloading $BASEURL/templates/common/upgrade_release.sh"
curl $OPTS $BASEURL/templates/common/upgrade_release.sh -o "upgrade_release.sh"
chmod 755 upgrade_release.sh

## download copy_static.escript
echo "Downloading $BASEURL/templates/common/copy_static.escript"
curl $OPTS $BASEURL/templates/common/copy_static.escript -o "copy_static.escript"
chmod 755 copy_static.escript

## download make_version_file.escript
echo "Downloading $BASEURL/templates/common/make_version_file.escript"
curl $OPTS $BASEURL/templates/common/make_version_file.escript -o "make_version_file.escript"
chmod 755 make_version_file.escript

## download in-git.sh
echo "Downloading $BASEURL/templates/common/in-git.sh"
curl $OPTS $BASEURL/templates/common/in-git.sh -o "in-git.sh"
chmod 755 in-git.sh

## download new nitrogen/bin
echo "Downloading $BASEURL/templates/common/bin/nitrogen"
curl $OPTS $BASEURL/templates/common/bin/nitrogen -o "bin/nitrogen"
chmod 755 bin/nitrogen

## Get new plugins thing
echo "Downloading $BASEURL/templates/common/do-plugins.escript"
curl $OPTS $BASEURL/templates/common/do-plugins.escript -o "do-plugins.escript"
chmod 755 do-plugins.escript

## rewrite rebar.config based on current deps
echo "Downloading $BASEURL/scripts/update_config_to_rebar3.escript"
curl $OPTS $BASEURL/scripts/update_config_to_rebar3.escript -o "update_config_to_rebar3.escript"
chmod 755 update_config_to_rebar3.escript

## Get new plugins.config
echo "Downloading $BASEURL/templates/common/plugins.config"
curl $OPTS $BASEURL/templates/common/plugins.config -o "plugins.config"

## Get erlang_ls.config for Erlang_LS
echo "Downloading $BASEURL/templates/common/erlang_ls.config"
curl $OPTS $BASEURL/templates/common/erlang_ls.config -o "erlang_ls.config"


echo "Updating rebar.config"
./update_config_to_rebar3.escript

echo "*****************************************************************************"
echo "*            YOU'RE ALMOST FINISHED. You MUST now do the following          *"
echo "*****************************************************************************"
echo "*                                                                           *"
echo "* 1. Review the updated rebar.config to ensure your dependencies look good  *"
echo "*                                                                           *"
echo "* 2. Once the dependencies are good, run the following command:             *"
echo "*                                                                           *"
echo "*    make fix-deps                                                          *" 
echo "*                                                                           *"
echo "*****************************************************************************"
echo
read -n 1 -s -r -p "            Press any key to acknowledge the message above"
