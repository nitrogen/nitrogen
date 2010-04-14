all: compile

compile:
	@mkdir -p ebin
	@erl \
		-pa ./ebin \
		-make

clean:
	@rm -rf ./ebin/*.beam
	@rm -rf ./test_ebin/*.beam

test: compile
	@erl \
		-noshell \
		-pa ./ebin \
		-pa ./test_ebin \
		-s eunit_helper start \
		-s init stop