REBAR=`which rebar`

all: deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

test:
	@$(REBAR) eunit

clean: 
	@$(REBAR) clean

run: all
	erl -noinput -pa ./deps/cecho/ebin -pa ./ebin -s erlangEditor start +A 50
