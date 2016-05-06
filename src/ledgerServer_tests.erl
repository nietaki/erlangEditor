%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 27. Apr 2016 00:10
%%%-------------------------------------------------------------------
-module(ledgerServer_tests).
-author("nietaki").

-include_lib("eunit/include/eunit.hrl").
-include_lib("ledgerServer.hrl").

foo_server_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun server_is_alive/1,
        fun server_registers_a_client/1,
        fun server_registers_a_client_andGetsChangesSubmitted/1,
        fun server_registers_a_client_andGetsTwoChangesSubmitted/1,
        fun second_client_gets_updated_text/1
    ]}.

server_is_alive(Pid) ->
    fun() ->
        ?assertEqual(true, is_process_alive(Pid))
    end.

server_registers_a_client(_Pid) ->
    fun() ->
        %%% previous, non-api way of doing this, for reference
        %gen_server:call(Pid, register),
        %{state, 0, [], _ClientPids, []} = gen_server:call(Pid, get_state)
        
        ?assertEqual({ledger_head_state, 0, ""}, ledgerServer:register("Steve")),
        #ledger_state{head_id = 0, head_text=[], clients= ClientsMap, changes=[]} = ledgerServer:debug_get_state(),
        ?assert(maps:is_key(self(), ClientsMap)),
        ?assertEqual(#client_info{username = "Steve", last_seen_head = 0}, maps:get(self(), ClientsMap))
    end.
    
server_registers_a_client_andGetsChangesSubmitted(_Pid) ->
    fun() ->
        ledgerServer:register("Steve"),
        ledgerServer:submit_local_changes(self(), 0, [{insert_char, 0, $x}]),
        ?assertMatch(#ledger_state{head_id = 1, head_text = "x", clients= _Clients, changes=[{insert_char, 0, $x}]}, ledgerServer:debug_get_state()),
        expect_cast({local_changes_accepted,0,1}) 
    end.

server_registers_a_client_andGetsTwoChangesSubmitted(_Pid) ->
    fun() ->
        ledgerServer:register("Steve"),
        ledgerServer:submit_local_changes(self(), 0, [{insert_char, 0, $x}, {insert_char, 1, $y}]),
        ?assertMatch(#ledger_state{head_id=2, head_text="xy", clients=_Clients, changes=[{insert_char, 0, $x}, {insert_char, 1, $y}]}, ledgerServer:debug_get_state()),
        expect_cast({local_changes_accepted,0,2}) 
    end.

second_client_gets_updated_text(_Pid) ->
    fun() ->
        ledgerServer:register("Steve"),
        ledgerServer:submit_local_changes(self(), 0, [{insert_char, 0, $x}]),
        
        Proxy = server_proxy:start(ledgerServer),
        Response = server_proxy:call(Proxy, {register, "John"}),
        
        ?assertEqual({ledger_head_state, 1, "x"}, Response),
        #ledger_state{clients=Clients} = ledgerServer:debug_get_state(),
        ?assert(length(maps:keys(Clients)) =:= 2),
        server_proxy:kill(Proxy)
    end.

%% utility functions

expect_cast(CastMessage) ->
    receive
        Msg ->
            ?assertEqual({'$gen_cast', CastMessage}, Msg)
    after 10 ->
        ?assert(false)
    end.

setup() ->
    %?debugMsg("setup"),
    process_flag(trap_exit, true),
    {ok, Pid} = ledgerServer:start_link(),
    Pid.

cleanup(Pid) ->
    %?debugMsg("cleanup"),
    lib:flush_receive(),
    exit(Pid, kill), %% brutal kill!
    ?assertEqual(false, is_process_alive(Pid)).