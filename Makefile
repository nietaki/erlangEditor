REBAR=`which rebar`

all: deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

test:
	@$(REBAR) -v eunit

clean: 
	@$(REBAR) clean


server: all
	erl -pa ./ebin -sname server@localhost -setcookie pass -s ledgerServer start_link +A 50

client: all
	erl -noinput -sname client@localhost -setcookie pass -pa ./deps/cecho/ebin -pa ./ebin -s erlangEditor start +A 50

client2: all
	erl -noinput -sname client2@localhost -setcookie pass -pa ./deps/cecho/ebin -pa ./ebin -s erlangEditor start +A 50

client3: all
	erl -noinput -sname client3@localhost -setcookie pass -pa ./deps/cecho/ebin -pa ./ebin -s erlangEditor start +A 50


shell: all
	erl -pa ./ebin -sname shell@localhost -setcookie pass -s cluster_utils join_server_cluster

connect: all
	erl -noinput -pa ./ebin -sname connect@localhost -setcookie pass -s cluster_utils test_connecting_to_server
