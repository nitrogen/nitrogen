all: compile

compile:
	@mkdir -p ebin
	@erl \
		-pa ./ebin \
		-make

clean:
	@rm -rf ./ebin/*.beam

test: compile
	@erl \
		-noshell \
		-pa ./ebin \
		-s eunit_helper start \
		-s init stop