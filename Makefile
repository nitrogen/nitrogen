
NITROGEN_VERSION=2.2.0

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
thanks: get-deps
	perl support/list_thanks/list_thanks.pl

# COWBOY

slim_cowboy: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_cowboy.config)
	@(cd rel; ./make_slim.escript reltool.config)
	@($(MAKE) rel_inner_slim)
	@echo Generated a slim-release Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Cowboy.

rel_cowboy: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_cowboy.config)
	@($(MAKE) rel_inner_full)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Cowboy.

rel_cowboy_win: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_cowboy.config reltool_win.config)
	@($(MAKE) rel_inner_win)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Cowboy.

package_cowboy: rel_cowboy
	mkdir -p ./builds
	$(MAKE) link_docs
	tar -C rel -c nitrogen | gzip --best > ./builds/nitrogen-${NITROGEN_VERSION}-cowboy.tar.gz

package_cowboy_win: rel_cowboy_win copy_docs
	mkdir -p ./builds
	$(MAKE) copy_docs
	7za a -r -tzip ./builds/nitrogen-${NITROGEN_VERSION}-cowboy-win.zip ./rel/nitrogen/
	rm -fr ./rel/nitrogen

# INETS

slim_inets: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_inets.config)
	@(cd rel; ./make_slim.escript reltool.config)
	@($(MAKE) rel_inner_slim)
	@echo Generated a slim-release Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Inets.

rel_inets: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_inets.config)
	@($(MAKE) rel_inner_full)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Inets.

rel_inets_win: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_inets.config reltool_win.config)
	@($(MAKE) rel_inner_win)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Inets.

package_inets: rel_inets
	mkdir -p ./builds
	$(MAKE) link_docs
	tar -C rel -c nitrogen | gzip --best > ./builds/nitrogen-${NITROGEN_VERSION}-inets.tar.gz

package_inets_win: rel_inets_win copy_docs
	mkdir -p ./builds
	$(MAKE) copy_docs
	7za a -r -tzip ./builds/nitrogen-${NITROGEN_VERSION}-inets-win.zip ./rel/nitrogen/
	rm -fr ./rel/nitrogen



# MOCHIWEB

slim_mochiweb: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_mochiweb.config)
	@(cd rel; ./make_slim.escript reltool.config)
	@($(MAKE) rel_inner_slim)
	@echo Generated a slim-release Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Mochiweb.

rel_mochiweb: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_mochiweb.config)
	@($(MAKE) rel_inner_full)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Mochiweb.

rel_mochiweb_win: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_mochiweb.config reltool_win.config)
	@($(MAKE) rel_inner_win)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Mochiweb.

package_mochiweb: rel_mochiweb
	mkdir -p ./builds
	$(MAKE) link_docs
	tar -C rel -c nitrogen | gzip --best > ./builds/nitrogen-${NITROGEN_VERSION}-mochiweb.tar.gz

package_mochiweb_win: rel_mochiweb_win copy_docs
	mkdir -p ./builds
	$(MAKE) copy_docs
	7za a -r -tzip ./builds/nitrogen-${NITROGEN_VERSION}-mochiweb-win.zip ./rel/nitrogen/
	rm -fr ./rel/nitrogen

# WEBMACHINE

slim_webmachine: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_webmachine.config)
	@(cd rel; ./make_slim.escript reltool.config)
	@($(MAKE) rel_inner_slim)
	@echo Generated a slim-release Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Webmachine.

rel_webmachine: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_webmachine.config)
	@($(MAKE) rel_inner_full)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Webmachine.

rel_webmachine_win: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_webmachine.config reltool_win.config)
	@($(MAKE) rel_inner_win)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Webmachine.

package_webmachine: rel_webmachine
	mkdir -p ./builds
	$(MAKE) link_docs
	tar -C rel -c nitrogen | gzip --best > ./builds/nitrogen-${NITROGEN_VERSION}-webmachine.tar.gz

package_webmachine_win: rel_webmachine_win copy_docs
	mkdir -p ./builds
	$(MAKE) copy_docs
	7za a -r -tzip ./builds/nitrogen-${NITROGEN_VERSION}-webmachine-win.zip ./rel/nitrogen/
	rm -fr ./rel/nitrogen

# YAWS

slim_yaws: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_yaws.config)
	@(cd rel; ./make_slim.escript reltool.config)
	@($(MAKE) rel_inner_slim)
	@echo Generated a slim-release Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Yaws.

rel_yaws: compile
	@$(MAKE) clean_release
	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_yaws.config)
	@($(MAKE) rel_inner_full)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Yaws.

####
#### One day, this will work, but it is not this day :(
####
## rel_yaws_win: compile
## 	@$(MAKE) clean_release
## 	@(cd rel; ./add_overlay.escript reltool.config reltool_base.config reltool_yaws.config reltool_win.config)
## 	@($(MAKE) rel_inner_win)
## 	@echo Generated a self-contained Nitrogen project
## 	@echo in 'rel/nitrogen', configured to run on Yaws.

package_yaws: rel_yaws
	mkdir -p ./builds
	$(MAKE) link_docs
	tar -C rel -c nitrogen | gzip --best > ./builds/nitrogen-${NITROGEN_VERSION}-yaws.tar.gz

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
	@(cd rel/nitrogen; $(MAKE); $(MAKE) cookie)
	@printf "Nitrogen Version:\n${NITROGEN_VERSION}\n\n" > rel/nitrogen/BuildInfo.txt
	@echo "Built On (uname -v):" >> rel/nitrogen/BuildInfo.txt
	@uname -v >> rel/nitrogen/BuildInfo.txt
	@cp -r ./deps/nitrogen_core/www rel/nitrogen/site/static/nitrogen
	@rm -rf rel/reltool.config	

rel_inner_slim: generate rel_inner

rel_inner_full: generate erl_interface rel_inner


rel_inner_win: generate erl_interface
	@(cd rel/nitrogen; cp releases/${NITROGEN_VERSION}/start_clean.boot bin/)
	@(cd rel/nitrogen; $(MAKE); $(MAKE) cookie)
	@(cd rel/nitrogen; ./make_start_cmd.sh)
	@printf "Nitrogen Version:\n${NITROGEN_VERSION}\n\n" > rel/nitrogen/BuildInfo.txt
	@echo "Built On (uname -v):" >> rel/nitrogen/BuildInfo.txt
	@uname -v >> rel/nitrogen/BuildInfo.txt
	@cp -r ./deps/nitrogen_core/www rel/nitrogen/site/static/nitrogen
	@rm -rf rel/reltool.config rel/nitrogen/make_start_cmd.sh rel/nitrogen/start.cmd.src

rel_copy_quickstart:
	cp -R ../NitrogenProject.com/src/* rel/nitrogen/site/src
	cp -R ../NitrogenProject.com/static/* rel/nitrogen/site/static
	cp -R ../NitrogenProject.com/templates/* rel/nitrogen/site/templates
	rm -rf rel/nitrogen/site/src/nitrogen_website.app.src
	(cd rel/nitrogen; ln -s site/static static)
	(cd rel/nitrogen; ln -s site/templates templates)

rellink:  
	$(foreach app,$(wildcard deps/*), rm -rf rel/nitrogen/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) rel/nitrogen/lib;)


