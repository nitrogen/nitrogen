
NITROGEN_VERSION=2.4.0

# If project name is not provided, just use 'myapp'
PROJECT?=myapp
PREFIX?=..

help:
	@echo 
	@echo "Usage: "
	@echo "       $(MAKE) {compile|clean}"        
	@echo
	@echo "       $(MAKE) {slim_cowboy|rel_cowboy|package_cowboy}"
	@echo "       $(MAKE) {slim_inets|rel_inets|package_inets}"  
	@echo "       $(MAKE) {slim_mochiweb|rel_mochiweb|package_mochiweb}"
	@echo "       $(MAKE) {slim_webmachine|rel_webmachine|package_webmachine}"
	@echo "       $(MAKE) {slim_yaws|rel_yaws|package_yaws}"
	@echo
	@echo "Windows Users:"
	@echo "       $(MAKE) rel_cowboy_win"
	@echo "       $(MAKE) rel_inets_win"
	@echo "       $(MAKE) rel_mochiweb_win"
	@echo "       $(MAKE) rel_webmachine_win"
	@echo
	@echo "To customize your project's name, add PROJECT=projectname"
	@echo "Example:"
	@echo "       $(MAKE) slim_yaws PROJECT=my_project"
	@echo
	@echo "To install the helper script on linux/unix machines"
	@echo "which allows you to invoke "nitrogen" or "dev" from any"
	@echo "directory in a Nitrogen installation."
	@echo "       $(MAKE) install-helper-script" 

all: get-deps compile

rebar:
	@(echo "Building rebar2 for your platform..")
	@(cd /tmp && \
	git clone https://github.com/choptastic/rebar && \
	cd rebar && \
	./bootstrap)
	@(echo "Moving rebar executable into your Nitrogen directory")
	@(mv /tmp/rebar/rebar .)
	@(echo "Cleaning up rebar remnants")
	@(cd /tmp && rm -fr rebar)
 

distribute-rebar: rebar
	@(cp rebar rel/rebar; cp rebar rel/overlay/common;)

get-deps: distribute-rebar
	./rebar get-deps

update-deps:
	./rebar update-deps

compile: get-deps
	./rebar compile

clean:
	./rebar clean

install-helper-script:
	@(cd support/helper_script;./install.sh)

install-vim-script:
	@(cd support/vim_rules/; ./install_rules.sh)
	@echo "Added vim rules to ~/.vimrc"

## Produce a list of contributors from the main repo and the dependent repos
thanks: 
	@(cd support/list_thanks; \
	rm -fr simple_bridge nprocreg nitrogen_core NitrogenProject.com; \
	git clone git://github.com/nitrogen/simple_bridge; \
	git clone git://github.com/nitrogen/nprocreg; \
	git clone git://github.com/nitrogen/nitrogen_core; \
	git clone git://github.com/nitrogen/NitrogenProject.com; \
	git clone git://github.com/nitrogen/rekt; \
	perl list_thanks.pl >> ../../thanks.txt; \
	rm -fr simple_bridge nprocreg nitrogen_core NitrogenProject.com; \
	echo "Thanks file generated in thanks.txt - please review")
	
quickstart:
	@($(MAKE) slim_mochiweb PROJECT=nitrogenproject_com PREFIX="$(PREFIX)")
	@($(MAKE) rel_copy_quickstart PROJECT=nitrogenproject_com PREFIX="$(PREFIX)")	
	@(cd "$(PREFIX)/nitrogenproject_com";$(MAKE))

quickstart_win: rel_mochiweb_win rel_copy_quickstart
	@($(MAKE) rel_copy_quickstart PROJECT=nitrogenproject_com PREFIX="$(PREFIX)")
	@(cd "$(PREFIX)/nitrogenproject_com";$(MAKE))

# COWBOY

slim_cowboy:
	@($(MAKE) slim PLATFORM=cowboy PROJECT=$(PROJECT))

rel_cowboy:
	@($(MAKE) rel PLATFORM=cowboy PROJECT=$(PROJECT))

rel_cowboy_win:
	@($(MAKE) rel_win PLATFORM=cowboy PROJECT=$(PROJECT))

package_cowboy: 
	@($(MAKE) package PLATFORM=cowboy PROJECT=$(PROJECT))

package_cowboy_win:
	@($(MAKE) package_win PLATFORM=cowboy PROJECT=$(PROJECT))


# INETS

slim_inets:
	@($(MAKE) slim PLATFORM=inets PROJECT=$(PROJECT))

rel_inets:
	@($(MAKE) rel PLATFORM=inets PROJECT=$(PROJECT))

rel_inets_win:
	@($(MAKE) rel_win PLATFORM=inets PROJECT=$(PROJECT))

package_inets: 
	@($(MAKE) package PLATFORM=inets PROJECT=$(PROJECT))

package_inets_win:
	@($(MAKE) package_win PLATFORM=inets PROJECT=$(PROJECT))


# MOCHIWEB

slim_mochiweb:
	@($(MAKE) slim PLATFORM=mochiweb PROJECT=$(PROJECT))

rel_mochiweb:
	@($(MAKE) rel PLATFORM=mochiweb PROJECT=$(PROJECT))

rel_mochiweb_win:
	@($(MAKE) rel_win PLATFORM=mochiweb PROJECT=$(PROJECT))

package_mochiweb: 
	@($(MAKE) package PLATFORM=mochiweb PROJECT=$(PROJECT))

package_mochiweb_win:
	@($(MAKE) package_win PLATFORM=mochiweb PROJECT=$(PROJECT))


# WEBMACHINE

slim_webmachine:
	@($(MAKE) slim PLATFORM=webmachine PROJECT=$(PROJECT))

rel_webmachine:
	@($(MAKE) rel PLATFORM=webmachine PROJECT=$(PROJECT))

rel_webmachine_win:
	@($(MAKE) rel_win PLATFORM=webmachine PROJECT=$(PROJECT))

package_webmachine: 
	@($(MAKE) package PLATFORM=webmachine PROJECT=$(PROJECT))

package_webmachine_win:
	@($(MAKE) package_win PLATFORM=webmachine PROJECT=$(PROJECT))


# YAWS

slim_yaws:
	@($(MAKE) slim PLATFORM=yaws PROJECT=$(PROJECT))

rel_yaws:
	@($(MAKE) rel PLATFORM=yaws PROJECT=$(PROJECT))

rel_yaws_win:
	@($(MAKE) rel_win PLATFORM=yaws PROJECT=$(PROJECT))

package_yaws: 
	@($(MAKE) package PLATFORM=yaws PROJECT=$(PROJECT))

package_yaws_win:
	@($(MAKE) package_win PLATFORM=yaws PROJECT=$(PROJECT))


# PLATFORM-AGNOSTIC

## OSX doesn't ship with a decent readlink, so this is a workaround
READLINK="support/readlink/readlink-f.sh"

move_release:
ifneq ($(shell $(READLINK) "$(PREFIX)/$(PROJECT)"), $(shell $(READLINK) "rel/nitrogen"))
	@(mkdir -p $(shell dirname "$(PREFIX)/$(PROJECT)"))
	@(mv ./rel/nitrogen $(PREFIX)/$(PROJECT))
endif

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
slim: check_exists compile
	@$(MAKE) clean_release
	@echo "********************************************************************************"
	@echo "Creating slim release in $(PREFIX)/$(PROJECT) with $(PLATFORM)"
	@echo "********************************************************************************"
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_slim.config)
	@($(MAKE) rel_inner_slim PLATFORM=$(PLATFORM))
	@($(MAKE) replace_project_name PROJECT=$(PROJECT))
	@($(MAKE) move_release PROJECT=$(PROJECT) PREFIX=$(PREFIX))
	@echo "********************************************************************************"
	@echo Generated a slim-release Nitrogen project
	@echo in '$(PREFIX)/$(PROJECT)', configured to run on $(PLATFORM).
	@echo "********************************************************************************"

rel: check_exists compile
	@$(MAKE) clean_release
	@echo "********************************************************************************"
	@echo "Creating full release in $(PREFIX)/$(PROJECT) with $(PLATFORM)"
	@echo "********************************************************************************"
	@(cd rel;cp reltool_base.config reltool.config)
	@($(MAKE) rel_inner_full PLATFORM=$(PLATFORM))
	@($(MAKE) replace_project_name PROJECT=$(PROJECT))
	@($(MAKE) move_release PROJECT=$(PROJECT) PREFIX=$(PREFIX))
	@echo "********************************************************************************"
	@echo Generated a self-contained Nitrogen project
	@echo in '$(PREFIX)/$(PROJECT)', configured to run on $(PLATFORM).
	@echo "********************************************************************************"

rel_win: check_exists compile
	@$(MAKE) clean_release
	@echo "********************************************************************************"
	@echo "Creating full Windows release in $(PREFIX)/$(PROJECT) with $(PLATFORM)"
	@echo "********************************************************************************"
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_win.config)
	@($(MAKE) rel_inner_win PLATFORM=$(PLATFORM))
	@($(MAKE) replace_project_name PROJECT=$(PROJECT))
	@($(MAKE) move_release PROJECT=$(PROJECT) PREFIX=$(PREFIX))
	@echo "********************************************************************************"
	@echo Generated a self-contained Nitrogen project
	@echo in '$(PREFIX)/$(PROJECT)', configured to run on $(PLATFORM).
	@echo "********************************************************************************"

package: rel
	mkdir -p ./builds
	$(MAKE) link_docs PREFIX=$(PREFIX) PROJECT=$(PROJECT)
	tar cf ./builds/$(PROJECT)-${NITROGEN_VERSION}-$(PLATFORM).tar -C $(PREFIX) $(PROJECT)
	gzip --best ./builds/$(PROJECT)-${NITROGEN_VERSION}-$(PLATFORM).tar 
	rm -fr $(PREFIX)/$(PROJECT)

package_win: rel_win copy_docs
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

clean_release:
	@(rm -rf rel/nitrogen)
	@(rm -rf rel/reltool.config)

generate:
	@(cd rel; ./rebar generate)

erl_interface:
	@(cd rel; escript copy_erl_interface.escript)

rel_inner:
	@(cd rel; ./merge_platform_dependencies.escript overlay/rebar.config.src overlay/$(PLATFORM).deps nitrogen/rebar.config)
	@(cd rel/nitrogen/etc; sed 's/BACKEND/$(PLATFORM)/' < simple_bridge.config > simple_bridge.temp)
	@(cd rel/nitrogen/etc; mv simple_bridge.temp simple_bridge.config)
	@(cd rel/nitrogen; $(MAKE); $(MAKE) cookie; $(MAKE) copy-static)
	@printf "Nitrogen Version:\n${NITROGEN_VERSION}\n\n" > "rel/nitrogen/BuildInfo.txt"
	@echo "Built On (uname -v):" >> "rel/nitrogen/BuildInfo.txt"
	@uname -v >> "rel/nitrogen/BuildInfo.txt"
	@rm -rf rel/reltool.config

rel_inner_slim:
	@(cd rel; ./make_slim.escript reltool.config)
	@($(MAKE) generate rel_inner PLATFORM=$(PLATFORM))

rel_inner_full:
	@($(MAKE) generate erl_interface rel_inner PLATFORM=$(PLATFORM))

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


## This rather stupid looking workaround is because OSX throws odd
## stdin errors with sed and "-i" despite us not even using stdin.
## I really wish OSX would ship with some non-terrible UNIX utilities.
replace_project_name:
	@(sed "s/{{PROJECT}}/$(PROJECT)/g" < rel/nitrogen/etc/vm.args > rel/nitrogen/etc/temp.args)
	@(mv rel/nitrogen/etc/temp.args rel/nitrogen/etc/vm.args)

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


