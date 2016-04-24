all:
	rebar get-deps
	rebar compile
	erl -noinput -pa ./deps/cecho/ebin -pa ./ebin -s erlangEditor start +A 50
