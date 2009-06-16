# all: compile test

compile:
	erl \
		-pa ebin \
		-make

clean:
	rm -rf ./coverage/*.*
	rm -rf ./ebin/*.*
	rm -rf ./test/ebin/*.*

test: compile
	erl -noshell \
		-pa ebin \
		-pa test/ebin \
		-s test_suite test \
		-s init stop

coverage: compile
	git submodule init lib/coverize
	git submodule update lib/coverize
	make -C lib/coverize
	mkdir -p coverage
	erl -noshell \
		-pa ebin \
		-pa test/ebin \
		-pa lib/coverize/ebin \
		-s eunit_helper run_cover \
		-s init stop
