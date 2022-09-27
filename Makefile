.PHONY: all compile deps clean distclean test check_plt build_plt dialyzer cleanplt

all: deps compile

compile: deps
	 rebar3 compile

deps:
	test -d deps || rebar3 get-deps

clean:
	rebar3 clean

distclean: clean
	   rebar3 delete-deps

DIALYZER_APPS = kernel stdlib erts sasl eunit syntax_tools compiler crypto common_test
