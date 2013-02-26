.PHONY: all test clean
REBAR=./rebar

all:
		@$(REBAR) get-deps compile

get-deps:
		@$(REBAR) get-deps

edoc:
		@$(REBAR) doc

test:
		@$(REBAR) -C rebar.test.config get-deps compile
		@$(REBAR) -C rebar.test.config ct skip_deps=true

clean:
		@rm -rf deps/ ebin/ logs/

build_plt:
		@$(REBAR) build-plt

dialyzer:
		@$(REBAR) dialyze
