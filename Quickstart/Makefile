NITROGEN_SRC=..

compile: 
	@(cd ../apps/nitrogen; make compile)
	@(cd ../apps/simple_bridge; make compile)
	erl -make

clean:
	rm -rf ./ebin/*.beam