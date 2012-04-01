REBAR=./rebar

all:
		@$(REBAR) get-deps compile

get-deps:
		@$(REBAR) get-deps

edoc:
		@$(REBAR) doc

test:
		@rm -rf .eunit
			@mkdir -p .eunit
				@$(REBAR) skip_deps=true eunit

clean:
		@rm -rf deps/ ebin/ logs/

build_plt:
		@$(REBAR) build-plt

dialyzer:
		@$(REBAR) dialyze
