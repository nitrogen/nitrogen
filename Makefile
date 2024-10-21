NITROGEN_VERSION=3.0.0

# If project name is not provided, just use 'myapp'
PROJECT?=myapp
PREFIX?=..

all: help

# Check if rebar3.mk exists, and if not, download it
ifeq ("$(wildcard rebar3.mk)","")
$(shell curl -O https://raw.githubusercontent.com/choptastic/rebar3.mk/master/rebar3.mk)
endif

# rebar3.mk adds a new rebar3 rule to your Makefile
# (see https://github.com/choptastic/rebar3.mk) for full info
include rebar3.mk

help:
	@echo 
	@echo "Usage: "
	@echo "   This will guide you through the choices below and is"
	@echo "   recommended for users who don't have a lot of experience"
	@echo "   with Nitrogen:"
	@echo "       $(MAKE) build"
	@echo
	@echo "   The below instructions are shortcuts to creating a new project:"
	@echo "     Linux/Unix/OSX/WSL Users:"
	@echo "       $(MAKE) {slim_cowboy|rel_cowboy|package_cowboy}"
	@echo "       $(MAKE) {slim_inets|rel_inets|package_inets}"  
	@echo "       $(MAKE) {slim_mochiweb|rel_mochiweb|package_mochiweb}"
	@echo "       $(MAKE) {slim_webmachine|rel_webmachine|package_webmachine}"
	@echo "       $(MAKE) {slim_yaws|rel_yaws|package_yaws}"
	@echo
	@echo "     Windows Users:"
	@echo "       $(MAKE) rel_cowboy_win"
	@echo "       $(MAKE) rel_inets_win"
	@echo "       $(MAKE) rel_mochiweb_win"
	@echo "       $(MAKE) rel_webmachine_win"
	@echo
	@echo "   To customize your project's name, add PROJECT=projectname"
	@echo "   Example:"
	@echo "       $(MAKE) slim_yaws PROJECT=my_project"
	@echo
	@echo "   To update your .vimrc file to handle nitrogen files better."
	@echo "   Specifically, any file that has vim:ft=nitrogen in the modeline"
	@echo "       $(MAKE) install-vim-script" 
	@echo


build:
	@(./build_helper)

template: rebar3
	@(./install_templates)

install-helper-script:
	@(cd support/helper_script;./install.sh)

install-vim-script:
	@(cd support/vim_rules/; ./install_rules.sh)
	@echo "Added vim rules to ~/.vimrc"

## Produce a list of contributors from the main repo and the dependent repos
thanks: 
	@(cd support/list_thanks; \
	rm -fr simple_bridge mutagen nprocreg nitrogen_core canister rekt nitro_cache NitrogenProject.com; \
	git clone https://github.com/nitrogen/simple_bridge; \
	git clone https://github.com/nitrogen/nprocreg; \
	git clone https://github.com/nitrogen/nitrogen_core; \
	git clone https://github.com/nitrogen/mutagen; \
	git clone https://github.com/nitrogen/NitrogenProject.com; \
	git clone https://github.com/nitrogen/nitro_cache; \
	git clone https://github.com/nitrogen/canister; \
	git clone https://github.com/nitrogen/rekt; \
	perl list_thanks.pl >> ../../thanks.txt; \
	rm -fr simple_bridge mutagen nprocreg nitrogen_core canister rekt nitro_cache NitrogenProject.com; \
	echo "Thanks file generated in thanks.txt - please review")
	
#quickstart:
#	@($(MAKE) slim_mochiweb PROJECT=nitrogenproject_com PREFIX="$(PREFIX)")
#	@($(MAKE) rel_copy_quickstart PROJECT=nitrogenproject_com PREFIX="$(PREFIX)")	
#	@(cd "$(PREFIX)/nitrogenproject_com";$(MAKE))
#
#quickstart_win: rel_mochiweb_win rel_copy_quickstart
#	@($(MAKE) rel_copy_quickstart PROJECT=nitrogenproject_com PREFIX="$(PREFIX)")
#	@(cd "$(PREFIX)/nitrogenproject_com";$(MAKE))

# COWBOY

cowboy: slim_cowboy

full_cowboy: rel_cowboy

full_cowboy_win: rel_cowboy_win

slim_cowboy:
	@($(MAKE) slim PLATFORM=cowboy PROJECT=$(PROJECT))

rel_cowboy:
	@($(MAKE) rel PLATFORM=cowboy PROJECT=$(PROJECT))

rel_cowboy_win:
	@($(MAKE) rel_win PLATFORM=cowboy PROJECT=$(PROJECT))

slim_cowboy_win:
	@($(MAKE) slim_win PLATFORM=cowboy PROJECT=$(PROJECT))

package_cowboy: 
	@($(MAKE) package PLATFORM=cowboy PROJECT=$(PROJECT))

package_cowboy_win:
	@($(MAKE) package_win PLATFORM=cowboy PROJECT=$(PROJECT))


# INETS

inets: slim_inets

full_inets: rel_inets

full_inets: rel_inets_win

slim_inets:
	@($(MAKE) slim PLATFORM=inets PROJECT=$(PROJECT))

rel_inets:
	@($(MAKE) rel PLATFORM=inets PROJECT=$(PROJECT))

rel_inets_win:
	@($(MAKE) rel_win PLATFORM=inets PROJECT=$(PROJECT))

slim_inets_win:
	@($(MAKE) slim_win PLATFORM=inets PROJECT=$(PROJECT))

package_inets: 
	@($(MAKE) package PLATFORM=inets PROJECT=$(PROJECT))

package_inets_win:
	@($(MAKE) package_win PLATFORM=inets PROJECT=$(PROJECT))


# MOCHIWEB

mochiweb: slim_mochiweb

full_mochiweb: rel_mochiweb

rel_mochiweb_: rel_mochiweb_win

slim_mochiweb:
	@($(MAKE) slim PLATFORM=mochiweb PROJECT=$(PROJECT))

rel_mochiweb:
	@($(MAKE) rel PLATFORM=mochiweb PROJECT=$(PROJECT))

rel_mochiweb_win:
	@($(MAKE) rel_win PLATFORM=mochiweb PROJECT=$(PROJECT))


slim_mochiweb_win:
	@($(MAKE) slim_win PLATFORM=mochiweb PROJECT=$(PROJECT))

package_mochiweb: 
	@($(MAKE) package PLATFORM=mochiweb PROJECT=$(PROJECT))

package_mochiweb_win:
	@($(MAKE) package_win PLATFORM=mochiweb PROJECT=$(PROJECT))


# WEBMACHINE

webmachine: slim_webmachine

full_webmachine: rel_webmachine

full_webmachine_win: rel_webmachine_win

slim_webmachine:
	@($(MAKE) slim PLATFORM=webmachine PROJECT=$(PROJECT))

rel_webmachine:
	@($(MAKE) rel PLATFORM=webmachine PROJECT=$(PROJECT))

rel_webmachine_win:
	@($(MAKE) rel_win PLATFORM=webmachine PROJECT=$(PROJECT))

slim_webmachine_win:
	@($(MAKE) slim_win PLATFORM=webmachine PROJECT=$(PROJECT))

package_webmachine: 
	@($(MAKE) package PLATFORM=webmachine PROJECT=$(PROJECT))

package_webmachine_win:
	@($(MAKE) package_win PLATFORM=webmachine PROJECT=$(PROJECT))


# YAWS

yaws: slim_yaws

full_yaws: rel_yaws

full_yaws_win: rel_yaws_win

slim_yaws:
	@($(MAKE) slim PLATFORM=yaws PROJECT=$(PROJECT))

rel_yaws:
	@($(MAKE) rel PLATFORM=yaws PROJECT=$(PROJECT))

rel_yaws_win:
	@($(MAKE) rel_win PLATFORM=yaws PROJECT=$(PROJECT))

slim_yaws_win:
	@($(MAKE) slim_win PLATFORM=yaws PROJECT=$(PROJECT))

package_yaws: 
	@($(MAKE) package PLATFORM=yaws PROJECT=$(PROJECT))

package_yaws_win:
	@($(MAKE) package_win PLATFORM=yaws PROJECT=$(PROJECT))


# PLATFORM-AGNOSTIC

## OSX doesn't ship with a decent readlink, so this is a workaround
READLINK="support/readlink/readlink-f.sh"

## move_release:
## ifneq ($(shell $(READLINK) "$(PREFIX)/$(PROJECT)"), $(shell $(READLINK) "rel/nitrogen"))
## 	@(mkdir -p $(shell dirname "$(PREFIX)/$(PROJECT)"))
## 	@(mv ./rel/nitrogen $(PREFIX)/$(PROJECT))
## endif

check_exists:
	@(test ! \( -e "$(PREFIX)/$(PROJECT)" \) || { echo "\n\
********************************************************************************\n\
ERROR: $(PREFIX)/$(PROJECT) exists.\n\
\n\
Please re-specify a project name with PROJECT=projectname and/or\n\
an installation directory with PREFIX=/path\n\
\n\
Exiting...\n\
********************************************************************************\n"; exit 1; })


## TODO: simplify further by adding a $(MODE) argument to be used in place of rel_inner_slim and rel_inner_full
slim: rebar3 check_exists template
	@echo "********************************************************************************"
	@echo "Creating a project that will default to a slim release"
	@echo "in $(PREFIX)/$(PROJECT) with $(PLATFORM)"
	@echo "********************************************************************************"
	$(REBAR) new nitrogen name=$(PROJECT) prefix=$(PREFIX) backend=$(PLATFORM)
	@(cd $(PREFIX)/$(PROJECT); make rebar2_links)
	@(cd $(PREFIX)/$(PROJECT); make cookie)
	@echo "********************************************************************************"
	@echo Generated a new Nitrogen project
	@echo in '$(PREFIX)/$(PROJECT)', configured to run on $(PLATFORM) with a slim release
	@echo "********************************************************************************"

rel: rebar3 check_exists template
	@echo "********************************************************************************"
	@echo "Creating a project that will default to a full release"
	@echo "in $(PREFIX)/$(PROJECT) with $(PLATFORM)"
	@echo "********************************************************************************"
	$(REBAR) new nitrogen name=$(PROJECT) prefix=$(PREFIX) backend=$(PLATFORM) include_erts=true
	@(cd $(PREFIX)/$(PROJECT); make rebar2_links)
	@(cd $(PREFIX)/$(PROJECT); make cookie)
	@echo "********************************************************************************"
	@echo Generated a new Nitrogen project
	@echo in '$(PREFIX)/$(PROJECT)', configured to run on $(PLATFORM) with a full release
	@echo "********************************************************************************"

slim_win: rebar3 check_exists template
	@echo "********************************************************************************"
	@echo "Creating a project that will default to a slim release"
	@echo "in $(PREFIX)/$(PROJECT) with $(PLATFORM) running on Windows"
	@echo "********************************************************************************"
	$(REBAR) new nitrogen_win name=$(PROJECT) prefix=$(PREFIX) backend=$(PLATFORM) include_erts=false
	@(cd $(PREFIX)/$(PROJECT); make cookie; make deps)
	@echo "********************************************************************************"
	@echo Generated a new Windows-based Nitrogen project
	@echo in '$(PREFIX)/$(PROJECT)', configured to run on $(PLATFORM) with a slim release
	@echo "********************************************************************************"

rel_win: rebar3 check_exists template
	@echo "********************************************************************************"
	@echo "Creating a project that will default to a full release"
	@echo "in $(PREFIX)/$(PROJECT) with $(PLATFORM) running on Windows"
	@echo "********************************************************************************"
	$(REBAR) new nitrogen_win name=$(PROJECT) prefix=$(PREFIX) backend=$(PLATFORM) include_erts=true
	@(cd $(PREFIX)/$(PROJECT); make cookie; make deps)
	@echo "********************************************************************************"
	@echo Generated a new Windows-based Nitrogen project
	@echo in '$(PREFIX)/$(PROJECT)', configured to run on $(PLATFORM) with a full release
	@echo "********************************************************************************"

package: rebar3
	mkdir -p ./builds
	$(MAKE) rel_$(PLATFORM) PREFIX=./builds PROJECT=nitrogen
	#$(MAKE) link_docs PREFIX=$(PREFIX) PROJECT=$(PROJECT)
	tar cf ./builds/$(PROJECT)-${NITROGEN_VERSION}-$(PLATFORM).tar -C $(PREFIX) $(PROJECT)
	gzip --best ./builds/$(PROJECT)-${NITROGEN_VERSION}-$(PLATFORM).tar 
	rm -fr $(PREFIX)/$(PROJECT)

package_win: rebar3 rel_win copy_docs
	mkdir -p ./builds
	7za a -r -tzip ./builds/$(PROJECT)-${NITROGEN_VERSION}-$(PLATFORM)-win.zip $(PREFIX)/$(PROJECT)/
	rm -fr $(PREFIX)/$(PROJECT)

# MASS PACKAGING - Produce packages for all servers

package_all: clean update-deps
	$(MAKE) package_inets package_mochiweb package_cowboy package_yaws package_webmachine PROJECT=nitrogen PREFIX=/tmp

package_all_win: clean update-deps
	$(MAKE) package_inets_win package_mochiweb_win package_cowboy_win package_webmachine_win PROJECT=nitrogen PREFIX=/tmp

clean_docs:
	@(cd $(PREFIX)/$(PROJECT); rm -fr doc)

copy_docs: clean_docs
	@(echo "Copying Documentation to the release")
	@(cd "$(PREFIX)/$(PROJECT)"; cp -r lib/nitrogen_core/doc .; cd doc; rm *.pl *.html)

link_docs: clean_docs
	@(echo "Linking Documentation in the release")
	@(cd "$(PREFIX)/$(PROJECT)"; ln -s lib/nitrogen_core/doc doc)

# TRAVIS-CI STUFF

ERLANG_VERSION_CHECK := erl -eval "io:format(\"~s\",[erlang:system_info(otp_release)]), halt()."  -noshell
ERLANG_VERSION = $(shell $(ERLANG_VERSION_CHECK))

# This is primarily for Travis build testing, as each build instruction will overwrite the previous
travis:
	$(MAKE) $(ERLANG_VERSION) PREFIX=rel PROJECT=nitrogen

base_make_all:
	$(MAKE) slim_inets
	$(MAKE) clean_release
	$(MAKE) slim_yaws
	$(MAKE) clean_release
	$(MAKE) slim_mochiweb
	$(MAKE) clean_release
	$(MAKE) slim_webmachine
	$(MAKE) clean_release

17: base_make_all
18: base_make_all
19: base_make_all
	$(MAKE) rel_cowboy
	$(MAKE) clean_release
	$(MAKE) slim_cowboy
	$(MAKE) clean_release
20: 19
21: 19
22: 19
23: 19

# SHARED

## clean_release:
## 	@(rm -rf rel/nitrogen)
## 	@(rm -rf rel/relx.config)

## generate:
## 	@(cd rel; ./rebar generate)
## 
## erl_interface:
## 	@(cd rel; escript copy_erl_interface.escript)

rel_inner:
	@(cd rel; ./merge_platform_dependencies.escript overlay/rebar.config.src overlay/$(PLATFORM).deps nitrogen/rebar.config)
	@(cd rel/nitrogen/etc; sed 's/BACKEND/$(PLATFORM)/' < simple_bridge.config > simple_bridge.temp)
	@(cd rel/nitrogen/etc; mv simple_bridge.temp simple_bridge.config)
	@(cd rel/nitrogen; $(MAKE); $(MAKE) cookie; $(MAKE) copy-static)
	@printf "Nitrogen Version:\n${NITROGEN_VERSION}\n\n" > "rel/nitrogen/BuildInfo.txt"
	@echo "Built On (uname -v):" >> "rel/nitrogen/BuildInfo.txt"
	@uname -v >> "rel/nitrogen/BuildInfo.txt"
	@rm -rf rel/reltool.config

rel_inner_win: generate erl_interface
	@# In OTP 21, start_clean.boot became no_dot_erlang.boot
	@(cd "rel/nitrogen"; cp releases/${NITROGEN_VERSION}/start_clean.boot bin/no_dot_erlang.boot)
	@(cd rel; ./merge_platform_dependencies.escript overlay/rebar.config.src overlay/$(PLATFORM).deps nitrogen/rebar.config)
	@(cd rel/nitrogen/etc; sed -i 's/BACKEND/$(PLATFORM)/' simple_bridge.config)
	@(cd rel/nitrogen; $(MAKE); $(MAKE) cookie; $(MAKE) copy-static)
	@(cd rel/nitrogen; ./make_start_cmd.sh)
	@printf "Nitrogen Version:\n${NITROGEN_VERSION}\n\n" > rel/nitrogen/BuildInfo.txt
	@echo "Built On (uname -v):" >> rel/nitrogen/BuildInfo.txt
	@uname -v >> rel/nitrogen/BuildInfo.txt
	@rm -rf rel/reltool.config rel/nitrogen/make_start_cmd.sh rel/nitrogen/start.cmd.src


## ## This rather stupid looking workaround is because OSX throws odd
## ## stdin errors with sed and "-i" despite us not even using stdin.
## ## I really wish OSX would ship with some non-terrible UNIX utilities.
## replace_project_name:
## 	@(sed "s/{{PROJECT}}/$(PROJECT)/g" < rel/nitrogen/etc/vm.args > rel/nitrogen/etc/temp.args)
## 	@(mv rel/nitrogen/etc/temp.args rel/nitrogen/etc/vm.args)

rel_copy_quickstart:
	mkdir -p deps
	(rm -fr deps/NitrogenProject.com)
	(cd deps; git clone -b ws git://github.com/nitrogen/NitrogenProject.com.git)
	cp -R deps/NitrogenProject.com/src/* "$(PREFIX)/$(PROJECT)/site/src"
	cp -R deps/NitrogenProject.com/static/* "$(PREFIX)/$(PROJECT)/site/static"
	cp -R deps/NitrogenProject.com/templates/* "$(PREFIX)/$(PROJECT)/site/templates"
	rm -rf "$(PREFIX)/$(PROJECT)/site/src/nitrogen_website.app.src"
	(cd "$(PREFIX)/$(PROJECT)"; ln -s site/static static)
	(cd "$(PREFIX)/$(PROJECT)"; ln -s site/templates templates)

rellink:  
	$(foreach app,$(wildcard deps/*), rm -rf rel/nitrogen/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) rel/nitrogen/lib;)


