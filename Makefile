
NITROGEN_VERSION=2.2.1

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
	@echo "To install the helper script on linux/unix machines"
	@echo "which allows you to invoke "nitrogen" or "dev" from any"
	@echo "directory in a Nitrogen installation."
	@echo "       $(MAKE) install-helper-script" 

all: get-deps compile

distribute-rebar:
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

## Produce a list of contributors from the main repo and the dependent repos
thanks: 
	@(cd support/list_thanks; \
	rm -fr simple_bridge nprocreg nitrogen_core NitrogenProject.com; \
	git clone git://github.com/nitrogen/simple_bridge; \
	git clone git://github.com/nitrogen/nprocreg; \
	git clone git://github.com/nitrogen/nitrogen_core; \
	git clone git://github.com/nitrogen/NitrogenProject.com; \
	perl list_thanks.pl >> ../../thanks.txt; \
	rm -fr simple_bridge nprocreg nitrogen_core NitrogenProject.com; \
	echo "Thanks file generated in thanks.txt - please review")
	
quickstart: rel_mochiweb rel_copy_quickstart
	@(cd rel/nitrogen;$(MAKE))

quickstart_win: rel_mochiweb_win rel_copy_quickstart
	@(cd rel/nitrogen;$(MAKE))

# COWBOY

slim_cowboy:
	@($(MAKE) slim PLATFORM=cowboy)

rel_cowboy:
	@($(MAKE) rel PLATFORM=cowboy)

rel_cowboy_win:
	@($(MAKE) rel_win PLATFORM=cowboy)

package_cowboy: 
	@($(MAKE) package PLATFORM=cowboy)

package_cowboy_win:
	@($(MAKE) package_win PLATFORM=cowboy)


# INETS

slim_inets:
	@($(MAKE) slim PLATFORM=inets)

rel_inets:
	@($(MAKE) rel PLATFORM=inets)

rel_inets_win:
	@($(MAKE) rel_win PLATFORM=inets)

package_inets: 
	@($(MAKE) package PLATFORM=inets)

package_inets_win:
	@($(MAKE) package_win PLATFORM=inets)


# MOCHIWEB

slim_mochiweb:
	@($(MAKE) slim PLATFORM=mochiweb)

rel_mochiweb:
	@($(MAKE) rel PLATFORM=mochiweb)

rel_mochiweb_win:
	@($(MAKE) rel_win PLATFORM=mochiweb)

package_mochiweb: 
	@($(MAKE) package PLATFORM=mochiweb)

package_mochiweb_win:
	@($(MAKE) package_win PLATFORM=mochiweb)


# WEBMACHINE

slim_webmachine:
	@($(MAKE) slim PLATFORM=webmachine)

rel_webmachine:
	@($(MAKE) rel PLATFORM=webmachine)

rel_webmachine_win:
	@($(MAKE) rel_win PLATFORM=webmachine)

package_webmachine: 
	@($(MAKE) package PLATFORM=webmachine)

package_webmachine_win:
	@($(MAKE) package_win PLATFORM=webmachine)


# YAWS

slim_yaws:
	@($(MAKE) slim PLATFORM=yaws)

rel_yaws:
	@($(MAKE) rel PLATFORM=yaws)

rel_yaws_win:
	@($(MAKE) rel_win PLATFORM=yaws)

package_yaws: 
	@($(MAKE) package PLATFORM=yaws)

package_yaws_win:
	@($(MAKE) package_win PLATFORM=yaws)


# PLATFORM-AGNOSTIC

## TODO: simplify further by adding a $(MODE) argument to be used in place of rel_inner_slim and rel_inner_full
slim: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_$(PLATFORM).config)
	@($(MAKE) rel_inner_slim PLATFORM=$(PLATFORM))
	@echo Generated a slim-release Nitrogen project
	@echo in 'rel/nitrogen', configured to run on $(PLATFORM).

rel: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_$(PLATFORM).config)
	@($(MAKE) rel_inner_full PLATFORM=$(PLATFORM))
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on $(PLATFORM).

rel_win: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_$(PLATFORM).config reltool_win.config)
	@($(MAKE) rel_inner_win PLATFORM=$(PLATFORM))
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on $(PLATFORM).

package: rel
	mkdir -p ./builds
	$(MAKE) link_docs
	tar cf ./builds/nitrogen-${NITROGEN_VERSION}-$(PLATFORM).tar -C rel nitrogen
	gzip --best ./builds/nitrogen-${NITROGEN_VERSION}-$(PLATFORM).tar 

package_win: rel_win copy_docs
	mkdir -p ./builds
	$(MAKE) copy_docs
	7za a -r -tzip ./builds/nitrogen-${NITROGEN_VERSION}-$(PLATFORM)-win.zip ./rel/nitrogen/
	rm -fr ./rel/nitrogen

# MASS PACKAGING - Produce packages for all servers

package_all: clean update-deps package_inets package_mochiweb package_cowboy package_yaws package_webmachine

package_all_win: clean update-deps package_inets_win package_mochiweb_win package_cowboy_win package_webmachine_win

clean_docs:
	@(cd rel/nitrogen; rm -fr doc)

copy_docs: clean_docs
	@(echo "Copying Documentation to the release")
	@(cd rel/nitrogen; cp -r lib/nitrogen_core/doc .; cd doc; rm *.pl *.html)

link_docs: clean_docs
	@(echo "Linking Documentation in the release")
	@(cd rel/nitrogen; ln -s lib/nitrogen_core/doc doc)

# TRAVIS-CI STUFF

ERLANG_VERSION_CHECK := erl -eval "io:format(\"~s\",[erlang:system_info(otp_release)]), halt()."  -noshell
ERLANG_VERSION = $(shell $(ERLANG_VERSION_CHECK))

# This is primarily for Travis build testing, as each build instruction will overwrite the previous
travis: $(ERLANG_VERSION)

R14B02: rel_inets rel_yaws rel_mochiweb rel_webmachine
R14B03: R14B02
R15B: R14B02 rel_cowboy
R15B01: R15B
R15B02: R15B slim_cowboy slim_inets slim_yaws slim_mochiweb slim_webmachine
R15B03: R15B02
R16B: R15B02
R16B01: R16B
R16B02: R16B

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
	@(cd rel/nitrogen; $(MAKE); $(MAKE) cookie; $(MAKE) copy-static)
	@printf "Nitrogen Version:\n${NITROGEN_VERSION}\n\n" > rel/nitrogen/BuildInfo.txt
	@echo "Built On (uname -v):" >> rel/nitrogen/BuildInfo.txt
	@uname -v >> rel/nitrogen/BuildInfo.txt
	@rm -rf rel/reltool.config	

rel_inner_slim:
	@(cd rel; ./make_slim.escript reltool.config)
	@($(MAKE) generate rel_inner PLATFORM=$(PLATFORM))

rel_inner_full: generate erl_interface rel_inner


rel_inner_win: generate erl_interface
	@(cd rel/nitrogen; cp releases/${NITROGEN_VERSION}/start_clean.boot bin/)
	@(cd rel; ./merge_platform_dependencies.escript overlay/rebar.config.src overlay/$(PLATFORM).deps nitrogen/rebar.config)
	@(cd rel/nitrogen; $(MAKE); $(MAKE) cookie; $(MAKE) copy-static)
	@(cd rel/nitrogen; ./make_start_cmd.sh)
	@printf "Nitrogen Version:\n${NITROGEN_VERSION}\n\n" > rel/nitrogen/BuildInfo.txt
	@echo "Built On (uname -v):" >> rel/nitrogen/BuildInfo.txt
	@uname -v >> rel/nitrogen/BuildInfo.txt
	@rm -rf rel/reltool.config rel/nitrogen/make_start_cmd.sh rel/nitrogen/start.cmd.src

rel_copy_quickstart:
	mkdir -p deps
	(rm -fr deps/NitrogenProject.com)
	(cd deps; git clone git://github.com/nitrogen/NitrogenProject.com.git)
	cp -R deps/NitrogenProject.com/src/* rel/nitrogen/site/src
	cp -R deps/NitrogenProject.com/static/* rel/nitrogen/site/static
	cp -R deps/NitrogenProject.com/templates/* rel/nitrogen/site/templates
	rm -rf rel/nitrogen/site/src/nitrogen_website.app.src
	(cd rel/nitrogen; ln -s site/static static)
	(cd rel/nitrogen; ln -s site/templates templates)

rellink:  
	$(foreach app,$(wildcard deps/*), rm -rf rel/nitrogen/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) rel/nitrogen/lib;)


