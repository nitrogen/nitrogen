# all: compile test

compile:
	@(cd ./apps/nitrogen; make compile)
	@(cd ./apps/simple_bridge; make compile)

clean:
	@(cd ./apps/nitrogen; make clean)
	@(cd ./apps/simple_bridge; make clean)