.PHONY: fix-slim-release test

all: compile nitrogen_js_css

REBAR?=./rebar3

cookie:
	@echo Generating a default cookie in /etc/vm.args
	@(echo -setcookie `openssl rand -base64 50 | head -n1 | sed -e 's/[^a-zA-Z0-9]//g'` >> etc/vm.args)

compile:
	@($(REBAR) compile)

eunit: get-deps compile
	@($(REBAR) eunit)

nitrogen_js_css: priv/static/nitrogen

copy-static:
	@(rm -fr priv/static/nitrogen)
	@(mkdir -p priv/static/nitrogen)
	@(cp -r _build/default/lib/nitrogen_core/www/* priv/static/nitrogen/)

link-static:
	@(rm -fr priv/static/nitrogen)
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

priv/static/nitrogen: link-static

dialyzer:
	@($(REBAR) dialyzer)

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
