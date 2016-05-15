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
        fun second_client_gets_updated_text/1,
        fun clients_get_their_changes_on_text_update/1,
        fun client_gets_ledger_changed_message_if_they_post_changes_with_old_head_id/1
    ]}.

server_is_alive(Pid) ->
    fun() ->
        ?assertEqual(true, is_process_alive(Pid))
    end.

server_registers_a_client(_Pid) ->
    fun() ->
        ?assertEqual(#ledger_head_state{head_id = 0, head_text = ""}, ledgerServer:register("Steve")),
        #ledger_state{head_id = 0, head_text=[], clients=ClientsMap, changes=[]} = ledgerServer:debug_get_state(),
        ?assert(maps:is_key(self(), ClientsMap)),
        ?assertEqual(#client_info{username = "Steve", last_seen_head = 0, cursor_position = 0}, maps:get(self(), ClientsMap))
    end.
    
server_registers_a_client_andGetsChangesSubmitted(_Pid) ->
    fun() ->
        ledgerServer:register("Steve"),
        test_utils:expect_cast_of_type(cursor_positions_at_head),
        ledgerServer:submit_local_changes(self(), 0, [{insert_char, 0, $x}]),
        ?assertMatch(#ledger_state{head_id = 1, head_text = "x", clients= _Clients}, ledgerServer:debug_get_state()),
        test_utils:expect_cast({local_changes_accepted,0,1})
    end.

server_registers_a_client_andGetsTwoChangesSubmitted(_Pid) ->
    fun() ->
        ledgerServer:register("Steve"),
        test_utils:expect_cast_of_type(cursor_positions_at_head),
        ledgerServer:submit_local_changes(self(), 0, [{insert_char, 0, $x}, {insert_char, 1, $y}]),
        ?assertMatch(#ledger_state{head_id=2, head_text="xy", clients=_Clients}, ledgerServer:debug_get_state()),
        test_utils:expect_cast({local_changes_accepted,0,2}) 
    end.

second_client_gets_updated_text(_Pid) ->
    fun() ->
        ledgerServer:register("Steve"),
        test_utils:expect_cast_of_type(cursor_positions_at_head),
        ledgerServer:submit_local_changes(self(), 0, [{insert_char, 0, $x}]),
        
        Proxy = server_proxy:start(ledgerServer),
        Response = server_proxy:run_fun(Proxy, fun() -> ledgerServer:register("John") end),
        
        ?assertEqual(#ledger_head_state{ head_id = 1, head_text = "x"}, Response),
        #ledger_state{clients=Clients} = ledgerServer:debug_get_state(),
        ?assert(length(maps:keys(Clients)) =:= 2),
        server_proxy:kill(Proxy)
    end.

clients_get_their_changes_on_text_update(_Pid) ->
    fun() ->
        % Arrange
        Proxy = server_proxy:start(ledgerServer),
        server_proxy:run_fun(Proxy, fun() -> ledgerServer:register("John") end),
        ?assertEqual(cursor_positions_at_head, element(1, server_proxy:receive_a_cast_message(Proxy))),
        
        ledgerServer:register("Steve"),
        test_utils:expect_cast_of_type(cursor_positions_at_head),
        
        % Act
        ledgerServer:submit_local_changes(self(), 0, [{insert_char, 0, $x}]),
        
        % Assert
        test_utils:expect_cast({local_changes_accepted,0,1}),
        
        %?assertEqual({}, ledgerServer:debug_get_state()),
        ?assertMatch({ledger_changed, 0, [{insert_char, 0, $x}]}, server_proxy:receive_a_cast_message(Proxy)),
        
        % A cleanup ;)
        server_proxy:kill(Proxy)
    end.

client_gets_ledger_changed_message_if_they_post_changes_with_old_head_id(_Pid) ->
    fun() ->
        Proxy = server_proxy:start(ledgerServer),
        server_proxy:run_fun(Proxy, fun() -> ledgerServer:register("John") end),
        
        ledgerServer:register("Steve"),
        test_utils:expect_cast_of_type(cursor_positions_at_head),
        server_proxy:run_fun(Proxy, fun() -> ledgerServer:submit_local_changes(self(), 0, [{insert_char, 0, $x}]) end),
        ?assertEqual(cursor_positions_at_head, element(1, server_proxy:receive_a_cast_message(Proxy))),
        ?assertMatch({local_changes_accepted,0,1}, server_proxy:receive_a_cast_message(Proxy)),
        
        ledgerServer:submit_local_changes(self(), 0, [{insert_char, 0, $y}]),
        test_utils:expect_cast({ledger_changed, 0, [{insert_char, 0, $x}]}),
        
        server_proxy:kill(Proxy)
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