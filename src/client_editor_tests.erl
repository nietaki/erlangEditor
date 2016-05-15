%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2016 23:32
%%%-------------------------------------------------------------------
-module(client_editor_tests).
-author("nietaki").

%% API
-export([]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("client_editor.hrl").

client_editor_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun client_connects_to_server/1,
        fun client_sends_a_letter_to_the_server_and_gets_its_changes_accepted/1,
        fun client_works_with_two_separate_changes/1,
        fun client_works_with_two_changes_one_after_another/1,
        fun a_client_gets_other_clients_changes/1,
        fun changes_of_two_clients_get_merged/1,
        fun changes_of_two_clients_get_merged_alternate_order/1,
        fun changes_of_two_clients_get_merged_no_waiting/1
    ]}.

client_connects_to_server(ServerPid) ->
    fun() ->
        ?assertEqual(true, is_process_alive(ServerPid)),
        ClientPid = spawn_client(),
        ServerState = ledgerServer:debug_get_state(),
        ?assertEqual(1, maps:size(ServerState#ledger_state.clients)),
        kill_client(ClientPid)
    end.

client_sends_a_letter_to_the_server_and_gets_its_changes_accepted(_ServerPid) ->
    fun() ->
        ClientPid = spawn_client(),
        client_editor:send_char(ClientPid, $q),
        timer:sleep(10),
        ClientState = client_editor:debug_get_state(ClientPid), 
        ?assertMatch(#ledger_state{head_id = 1, head_text = "q"}, ledgerServer:debug_get_state()),
        
        % client's changes should already be accepted
        ?assertMatch(#ledger_head_state{head_id = 1, head_text = "q"}, ClientState#client_state.ledger_head_state),
        ?assertMatch(#local_state{changes = [], cursor_position = 1, resulting_text = "q"}, ClientState#client_state.local_state),
        kill_client(ClientPid)
    end.

client_works_with_two_separate_changes(_ServerPid) ->
    fun() ->
        ClientPid = spawn_client(),
        client_editor:send_char(ClientPid, $a),
        timer:sleep(10),
        client_editor:send_char(ClientPid, $b),
        timer:sleep(100),
        ClientState = client_editor:debug_get_state(ClientPid), 
        ?assertMatch(#ledger_state{head_id = 2, head_text = "ab"}, ledgerServer:debug_get_state()),
        
        % client's changes should already be accepted
        ?assertMatch(#ledger_head_state{head_id = 2, head_text = "ab"}, ClientState#client_state.ledger_head_state),
        ?assertMatch(#local_state{changes = [], cursor_position = 2, resulting_text = "ab"}, ClientState#client_state.local_state),
        kill_client(ClientPid)
    end.

client_works_with_two_changes_one_after_another(_ServerPid) ->
    fun() ->
        ClientPid = spawn_client(),
        client_editor:send_char(ClientPid, $a),
        client_editor:send_char(ClientPid, $b),
        timer:sleep(10),
        ClientState = client_editor:debug_get_state(ClientPid), 
        ?assertMatch(#ledger_state{head_id = 2, head_text = "ab"}, ledgerServer:debug_get_state()),
        
        % client's changes should already be accepted
        ?assertMatch(#ledger_head_state{head_id = 2, head_text = "ab"}, ClientState#client_state.ledger_head_state),
        ?assertMatch(#local_state{changes = [], cursor_position = 2, resulting_text = "ab"}, ClientState#client_state.local_state),
        kill_client(ClientPid)
    end.

a_client_gets_other_clients_changes(_ServerPid) ->
    fun() ->
        A = spawn_client(a),
        B = spawn_client(b),
        client_editor:send_char(A, $a),
        timer:sleep(10),
        AState = client_editor:debug_get_state(A),
        BState = client_editor:debug_get_state(B), 
        
        ?assertMatch(#ledger_state{head_id = 1, head_text = "a"}, ledgerServer:debug_get_state()),
        
        % client's changes should already be accepted
        ?assertMatch(#ledger_head_state{head_id = 1, head_text = "a"}, AState#client_state.ledger_head_state),
        ?assertMatch(#ledger_head_state{head_id = 1, head_text = "a"}, BState#client_state.ledger_head_state),
        ?assertMatch(#local_state{changes = [], cursor_position = 1, resulting_text = "a"}, AState#client_state.local_state),
        ?assertMatch(#local_state{changes = [], cursor_position = 1, resulting_text = "a"}, BState#client_state.local_state),
        kill_client(A),
        kill_client(B)
    end.

changes_of_two_clients_get_merged(_ServerPid) ->
    fun() ->
        A = spawn_client(a),
        B = spawn_client(b),
        client_editor:send_char(A, $X),
        client_editor:send_keystroke(A, left),
        timer:sleep(10),
        client_editor:send_char(A, $a),
        client_editor:send_char(B, $b),
        timer:sleep(20),
        
        AState = client_editor:debug_get_state(A),
        BState = client_editor:debug_get_state(B), 
        
        ?assertMatch(#ledger_state{head_id = 3, head_text = "aXb"}, ledgerServer:debug_get_state()),
        
        % client's changes should already be accepted
        ?assertMatch(#ledger_head_state{head_id = 3, head_text = "aXb"}, AState#client_state.ledger_head_state),
        ?assertMatch(#ledger_head_state{head_id = 3, head_text = "aXb"}, BState#client_state.ledger_head_state),
        kill_client(A),
        kill_client(B)
    end.

changes_of_two_clients_get_merged_alternate_order(_ServerPid) ->
    fun() ->
        A = spawn_client(a),
        B = spawn_client(b),
        client_editor:send_char(A, $X),
        client_editor:send_keystroke(A, left),
        timer:sleep(10),
        client_editor:send_char(B, $b),
        client_editor:send_char(A, $a),
        timer:sleep(20),
        
        AState = client_editor:debug_get_state(A),
        BState = client_editor:debug_get_state(B), 
        
        ?assertMatch(#ledger_state{head_id = 3, head_text = "aXb"}, ledgerServer:debug_get_state()),
        
        % client's changes should already be accepted
        ?assertMatch(#ledger_head_state{head_id = 3, head_text = "aXb"}, AState#client_state.ledger_head_state),
        ?assertMatch(#ledger_head_state{head_id = 3, head_text = "aXb"}, BState#client_state.ledger_head_state),
        kill_client(A),
        kill_client(B)
    end.

changes_of_two_clients_get_merged_no_waiting(_ServerPid) ->
    fun() ->
        A = spawn_client(a),
        B = spawn_client(b),
        client_editor:send_char(A, $X),
        client_editor:send_keystroke(A, left),
        client_editor:send_char(A, $a),
        client_editor:send_char(B, $b),
        timer:sleep(20),
        
        AState = client_editor:debug_get_state(A),
        BState = client_editor:debug_get_state(B), 
        
        ?assertMatch(#ledger_state{head_id = 3, head_text = "aXb"}, ledgerServer:debug_get_state()),
        
        % client's changes should already be accepted
        ?assertMatch(#ledger_head_state{head_id = 3, head_text = "aXb"}, AState#client_state.ledger_head_state),
        ?assertMatch(#ledger_head_state{head_id = 3, head_text = "aXb"}, BState#client_state.ledger_head_state),
        kill_client(A),
        kill_client(B)
    end.

% util functions
spawn_client() -> spawn_client(client_editor).

spawn_client(ArbitraryName) ->
    {ok, ClientPid} = client_editor:start_link(fun initializeNop/1, fun repaintNop/2, ArbitraryName),
    ClientPid.

kill_client(Pid) ->
    process_flag(trap_exit, true),
    exit(Pid, kill). %% brutal kill!

initializeNop(_) -> {ok, {10, 10}}.
repaintNop(_, _) -> ok.

setup() ->
    %?debugMsg("setup"),
    process_flag(trap_exit, true),
    {ok, ServerPid} = ledgerServer:start_link(),
    ServerPid.

cleanup(Pid) ->
    %?debugMsg("cleanup"),
    lib:flush_receive(),
    exit(Pid, kill), %% brutal kill!
    ?assertEqual(false, is_process_alive(Pid)).
