.PHONY: all compile deps clean distclean test check_plt build_plt dialyzer cleanplt

all: deps compile

compile: deps
	rebar3 compile
	cp -rf  _build/default/lib/erlang_craq/ebin/* ebin/

deps:
	test -d deps || rebar3 get-deps
ean:
	rebar3 clean
	rm ebin/*.beam

distclean: clean
	   rebar3 delete-deps

DIALYZER_APPS = kernel stdlib erts sasl eunit syntax_tools compiler crypto common_test
