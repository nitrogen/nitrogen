# all: compile test

compile:
	git submodule update --init
	@(cd ./deps/simple_bridge; make)
	@(cd ./deps/binary_vice; make)
	erl -make

clean:
	rm -rf ./coverage/*.*
	rm -rf ./ebin/*.beam
	rm -rf ./deps/*/ebin/*.beam
	rm -rf ./test/ebin/*.beam

test: compile
	erl -noshell \
		-pa ebin \
		-pa test/ebin \
		-s test_suite test \
		-s init stop

coverage: compile
	git submodule update --init
	make -C deps/coverize
	mkdir -p coverage
	erl -noshell \
		-pa ebin \
		-pa test/ebin \
		-pa deps/coverize/ebin \
		-s eunit_helper run_cover \
		-s init stop
