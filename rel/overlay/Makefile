all:
        # Compile using embedded Erlang if it exists. 
        # Otherwise try the system Erlang.
	@(cd site; [ -e ../erts-*/bin/erl ] && ../erts-*/bin/erl -make; echo)
	@(cd site; [ -e ../erts-*/bin/erl ] || erl -pa ../lib/*/ebin -make; echo)

clean:
	@(cd site; rm -rf ebin/*.beam)
