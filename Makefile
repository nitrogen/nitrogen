
NITROGEN_VERSION=2.1.0

help:
	@echo 
	@echo "Usage: "
	@echo "       ./make {compile|clean}"        
	@echo
	@echo "       ./make {rel_inets|package_inets}"  
	@echo "       ./make {rel_mochiweb|package_mochiweb}"
	@echo "       ./make {rel_webmachine|package_webmachine}"
	@echo "       ./make {rel_yaws|package_yaws}"
	@echo
	@echo

all: get-deps compile

get-deps:
	./rebar get-deps

compile: get-deps
	./rebar compile

clean:
	./rebar clean

# INETS

rel_inets: compile
	@rm -rf rel/nitrogen
	@rm -rf rel/reltool.config
	@ln rel/reltool_inets.config rel/reltool.config
	@(make rel_inner)
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
	@(cd rel/nitrogen; make)
	@printf "Nitrogen Version:\n${NITROGEN_VERSION}\n\n" > rel/nitrogen/BuildInfo.txt
	@echo "Built On (uname -v):" >> rel/nitrogen/BuildInfo.txt
	@uname -v >> rel/nitrogen/BuildInfo.txt
	@cp -r ./deps/nitrogen_core/www rel/nitrogen/site/static/nitrogen
	@rm -rf rel/reltool.config	

rel_copy_quickstart:
	cp -R ../NitrogenProject.com/src/* rel/nitrogen/site/src
	cp -R ../NitrogenProject.com/static/* rel/nitrogen/site/static
	cp -R ../NitrogenProject.com/templates/* rel/nitrogen/site/templates
	rm -rf rel/nitrogen/site/src/nitrogen_website.app.src
	(cd rel/nitrogen; ln -s site/static static)
	(cd rel/nitrogen; ln -s site/templates templates)

rellink:  
	$(foreach app,$(wildcard deps/*), rm -rf rel/nitrogen/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) rel/nitrogen/lib;)


