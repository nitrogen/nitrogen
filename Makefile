
NITROGEN_VERSION=2.0.0

help:
	@echo 
	@echo "Usage: ./make {compile|clean}                   # Compile" 
	@echo "       ./make {rel_inets|rel_mochiweb}          # Build release"
	@echo "       ./make {package_inets|package_mochiweb}  # Package .tar.gz"
	@echo


compile:
	@(cd ./apps/nitrogen; make compile)
	@(cd ./apps/simple_bridge; make compile)
	@(cd ./apps/mprocreg; make compile)


clean:
	@(cd ./apps/nitrogen; make clean)
	@(cd ./apps/simple_bridge; make clean)
	@(cd ./apps/mprocreg; make clean)


rel: rel_inets


rel_inets: compile
	@rm -rf rel/nitrogen
	@rm -rf rel/reltool.config
	@ln rel/inets.config rel/reltool.config
	@(cd rel; ./rebar generate)
	@(cd rel/nitrogen; make)
	@rm -rf rel/reltool.config	
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Inets.


rel_mochiweb: compile
	@rm -rf rel/nitrogen
	@rm -rf rel/reltool.config
	@ln rel/mochiweb.config rel/reltool.config
	@(cd rel; ./rebar generate)
	@(cd rel/nitrogen; make)
	@rm -rf rel/reltool.config	
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Mochiweb.


package_inets: 
	mkdir -p ./builds
	tar -C rel -c nitrogen | gzip > ./builds/nitrogen-${NITROGEN_VERSION}-inets.tar.gz


package_mochiweb: rel_mochiweb
	mkdir -p ./builds
	tar -C rel -c nitrogen | gzip > ./builds/nitrogen-${NITROGEN_VERSION}-mochiweb.tar.gz