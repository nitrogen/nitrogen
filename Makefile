
NITROGEN_VERSION=2.1.0

help:
	@echo 
	@echo "Usage: "
	@echo "       ./make {compile|clean}"        
	@echo
	@echo "       ./make {rel_cowboy|package_cowboy}"
	@echo "       ./make {rel_inets|package_inets}"  
	@echo "       ./make {rel_mochiweb|package_mochiweb}"
	@echo "       ./make {rel_webmachine|package_webmachine}"
	@echo "       ./make {rel_yaws|package_yaws}"
	@echo
	@echo "Windows Users:"
	@echo "       ./make rel_inets_win"
	@echo "       ./make rel_mochiweb_win"
	@echo
	@echo

all: get-deps compile

distribute-rebar:
	@(cp rebar rel/rebar; cp rebar rel/overlay/common;)

get-deps: distribute-rebar
	./rebar get-deps

compile: get-deps
	./rebar compile

clean:
	./rebar clean

# COWBOY

rel_cowboy: compile
	@rm -rf rel/nitrogen
	@rm -rf rel/reltool.config
	@ln rel/reltool_cowboy.config rel/reltool.config
	@(make rel_inner)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Cowboy.

package_cowboy: rel_cowboy
	mkdir -p ./builds
	tar -C rel -c nitrogen | gzip > ./builds/nitrogen-${NITROGEN_VERSION}-yaws.tar.gz

# INETS

rel_inets: compile
	@rm -rf rel/nitrogen
	@rm -rf rel/reltool.config
	@ln rel/reltool_inets.config rel/reltool.config
	@(make rel_inner)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Inets.

rel_inets_win: compile
	@rm -rf rel/nitrogen
	@rm -rf rel/reltool.config
	@ln rel/reltool_inets_win.config rel/reltool.config
	@(make rel_inner_win)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Inets.

package_inets: rel_inets
	mkdir -p ./builds
	tar -C rel -c nitrogen | gzip > ./builds/nitrogen-${NITROGEN_VERSION}-inets.tar.gz




# MOCHIWEB

rel_mochiweb: compile
	@rm -rf rel/nitrogen
	@rm -rf rel/reltool.config
	@ln rel/reltool_mochiweb.config rel/reltool.config
	@(make rel_inner)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Mochiweb.

rel_mochiweb_win: compile
	@rm -rf rel/nitrogen
	@rm -rf rel/reltool.config
	@ln rel/reltool_mochiweb_win.config rel/reltool.config
	@(make rel_inner_win)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Mochiweb.

package_mochiweb: rel_mochiweb
	mkdir -p ./builds
	tar -C rel -c nitrogen | gzip > ./builds/nitrogen-${NITROGEN_VERSION}-mochiweb.tar.gz

# WEBMACHINE

rel_webmachine: compile
	@rm -rf rel/nitrogen
	@rm -rf rel/reltool.config
	@ln rel/reltool_webmachine.config rel/reltool.config
	@(make rel_inner)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Webmachine.

package_webmachine: rel_webmachine
	mkdir -p ./builds
	tar -C rel -c nitrogen | gzip > ./builds/nitrogen-${NITROGEN_VERSION}-webmachine.tar.gz


# YAWS

rel_yaws: compile
	@rm -rf rel/nitrogen
	@rm -rf rel/reltool.config
	@ln rel/reltool_yaws.config rel/reltool.config
	@(make rel_inner)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Yaws.

package_yaws: rel_yaws
	mkdir -p ./builds
	tar -C rel -c nitrogen | gzip > ./builds/nitrogen-${NITROGEN_VERSION}-yaws.tar.gz


# SHARED


rel_inner:
	@(cd rel; ./rebar generate)
	@(cd rel; escript copy_erl_interface.escript)
	@(cd rel/nitrogen; make)
	@printf "Nitrogen Version:\n${NITROGEN_VERSION}\n\n" > rel/nitrogen/BuildInfo.txt
	@echo "Built On (uname -v):" >> rel/nitrogen/BuildInfo.txt
	@uname -v >> rel/nitrogen/BuildInfo.txt
	@cp -r ./deps/nitrogen_core/www rel/nitrogen/site/static/nitrogen
	@rm -rf rel/reltool.config	

rel_inner_win:
	@(cd rel; ./rebar generate)
	@(cd rel; escript copy_erl_interface.escript)
	@(cd rel/nitrogen; cp releases/${NITROGEN_VERSION}/start_clean.boot bin/)
	@(cd rel/nitrogen; make)
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


