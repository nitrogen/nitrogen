compile:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; ./rebar compile)

deps:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; ./rebar get-deps)

update_deps:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; ./rebar get-deps)

clean:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; ./rebar clean)
