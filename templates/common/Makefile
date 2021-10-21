.PHONY: fix-slim-release test

all: plugins compile nitrogen_js_css

REBAR?=./rebar3

cookie:
	@echo Generating a default cookie in /etc/vm.args
	@(echo -setcookie `openssl rand -base64 50 | head -n1 | sed -e 's/[^a-zA-Z0-9]//g'` >> etc/vm.args)

compile:
	@($(REBAR) compile)

get-deps:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; $(REBAR) get-deps)

update-deps:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; $(REBAR) update-deps)

eunit: get-deps compile
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Erlang in `which erl`"; $(REBAR) skip_deps=true eunit)

nitrogen_js_css: link-static

copy-static:
	@(mkdir -p priv/static/nitrogen)
	@(cp -r _build/default/lib/nitrogen_core/www/* priv/static/nitrogen/)

link-static:
	@(ln -s ../../_build/default/lib/nitrogen_core/www priv/static/nitrogen)

plugins:
	@(escript do-plugins.escript)

rebar2_links: site site/static site/src site/templates site/include

site:
	@echo Making Nitrogen 2 compatible directory structure symlinks
	@(mkdir -p site)

site/static:
	@(ln -s ../priv/static site/static)

site/src:
	@(ln -s ../src site/src)

site/templates:
	@(ln -s ../priv/templates site/templates)

site/include:
	@(ln -s ../include site/include)

site/static/nitrogen: link-static

#DEPS_PLT=$(CURDIR)/.deps_plt
#DEPS=erts kernel stdlib sasl lib/nitrogen_core lib/simple_bridge lib/nprocreg lib/nitro_cache lib/rekt lib/qdate
#
#$(DEPS_PLT):
#	@echo Building local plt at $(DEPS_PLT)
#	@echo 
#	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Dialyzer in `which dialyzer`"; dialyzer --output_plt $(DEPS_PLT) --build_plt --apps $(DEPS) -r ./lib)
#

dialyzer: all $(DEPS_PLT)
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; echo "Using Dialyzer in `which dialyzer`"; dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./site/ebin)

#update: update-deps copy-static compile
#	@(echo "*** CONGRATULATIONS ***")
#	@(echo "Your Nitrogen installation has been upgraded.")
#	@(echo "You may need to manually merge any changes that may have been made to")
#	@(echo "configuration files as well as the initialization modules:")
#	@(echo "    site/src/nitrogen_sup.erl")
#	@(echo "    site/src/nitrogen_PLATFORM.erl")
#	@(echo "    site/src/nitrogen_app.erl")
#	@(echo "")
#
#upgrade: update
#
#clean:
#	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH; $(REBAR) clean)
