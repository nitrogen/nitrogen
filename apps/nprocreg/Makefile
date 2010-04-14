all: compile

compile:
	@mkdir -p ebin
	@erl \
		-pa ./ebin \
		-make

clean:
	@rm -rf ./ebin/*.beam

run: compile
	erl -pa ./ebin -eval "application:start(nprocreg)."