.PHONY: fix-slim-release test

all: releases get-deps plugins compile

REBAR?=./rebar -q

cookie:
	@echo Generating a default cookie in /etc/vm.args
	@(echo -setcookie `openssl rand -base64 50 | head -n1 | sed -e 's/[^a-zA-Z0-9]//g'` >> etc/vm.args)

compile:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; $(REBAR) compile)

get-deps:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; $(REBAR) get-deps)

update-deps:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; $(REBAR) update-deps)

eunit: get-deps compile
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; $(REBAR) skip_deps=true eunit)

releases:
	make fix-slim-release

copy-static:
	@(mkdir -p site/static/nitrogen)
	@(cp -r lib/nitrogen_core/www/* site/static/nitrogen/)

plugins:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH;escript do-plugins.escript)

## This is only applicable for slim releases
fix-slim-release:
	@echo Fixing slim release to the version of Erlang installed on this machine
	@(./fix-slim-release)

DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib sasl nitrogen_core simple_bridge sync nprocreg

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo 
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Dialyzer in `which dialyzer`"; dialyzer --output_plt $(DEPS_PLT) --build_plt --apps $(DEPS))


dialyzer: all $(DEPS_PLT)
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Dialyzer in `which dialyzer`"; dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./site/ebin)

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
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; $(REBAR) clean)
