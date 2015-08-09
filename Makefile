ERL=erl
REBAR=./rebar
GIT = git
REBAR_VER = 2.6.0

.PHONY: get-deps

all: compile

compile:
	@$(REBAR) get-deps
	@$(REBAR) compile

rebar_src:
	@rm -rf $(PWD)/rebar_src
	@$(GIT) clone git://github.com/rebar/rebar.git rebar_src
	@$(GIT) -C rebar_src checkout tags/$(REBAR_VER)
	@cd $(PWD)/rebar_src/; ./bootstrap
	@cp $(PWD)/rebar_src/rebar $(PWD)
	@rm -rf $(PWD)/rebar_src

get-deps:
	@$(REBAR) get-deps

## dialyzer
PLT_FILE = ~/boss_db.plt
PLT_APPS ?= kernel stdlib erts
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions -Wunmatched_returns \
		-Wunderspecs --verbose --fullpath -n

.PHONY: dialyze
dialyze: all
	@[ -f $(PLT_FILE) ] || $(MAKE) plt
	@dialyzer --plt $(PLT_FILE) $(DIALYZER_OPTS) ebin || [ $$? -eq 2 ];

## In case you are missing a plt file for dialyzer,
## you can run/adapt this command
.PHONY: plt
plt:
	@echo "Building PLT, may take a few minutes"
	@dialyzer --build_plt --output_plt $(PLT_FILE) --apps \
		$(PLT_APPS) || [ $$? -eq 2 ];

clean:
	@$(REBAR) clean
	rm -fv erl_crash.dump
	rm -f $(PLT_FILE)

