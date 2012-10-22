all: get-deps compile

compile:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; ./rebar compile)

get-deps:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; ./rebar get-deps)

update-deps:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; ./rebar update-deps)

copy-static:
	@(cp -r lib/nitrogen_core/www/* site/static/nitrogen/)

update: update-deps copy-static compile
	@(echo "*** CONGRATULATIONS ***")
	@(echo "Your Nitrogen installation has been upgraded.")
	@(echo "You may need to manually merge any changes that may have been made to")
	@(echo "configuration files as well as the initialization modules:")
	@(echo "    site/src/nitrogen_sup.erl")
	@(echo "    site/src/nitrogen_PLATFORM.erl")
	@(echo "    site/src/nitrogen_app.erl")
	@(echo "")

upgrade: update

clean:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; ./rebar clean)
