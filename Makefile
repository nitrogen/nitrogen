# all: compile test

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




# rel_yaws: compile
# 	@rm -rf rel/nitrogen
# 	@rm -rf rel/reltool.config
# 	@ln rel/yaws.config rel/reltool.config
# 	@(cd rel; ./rebar generate)
# 	@(cd rel/nitrogen; make)
# 	@rm -rf rel/reltool.config	
# 	@echo Generated self-contained Nitrogen (Yaws) project in rel/nitrogen.


# rel_misultin: compile
# 	@rm -rf rel/nitrogen
# 	@rm -rf rel/reltool.config
# 	@ln rel/misultin.config rel/reltool.config
# 	@(cd rel; ./rebar generate)
# 	@(cd rel/nitrogen; make)
# 	@rm -rf rel/reltool.config	
# 	@echo Generated self-contained Nitrogen (Misultin) project in rel/nitrogen.