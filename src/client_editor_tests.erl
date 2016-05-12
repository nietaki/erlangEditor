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
        fun client_works_with_two_changes_one_after_another/1
    ]}.

client_connects_to_server(ServerPid) ->
    fun() ->
        ?assertEqual(true, is_process_alive(ServerPid)),
        ClientPid = spawn_client(),
        ServerState = ledgerServer:debug_get_state(),
        ?assertEqual(1, maps:size(ServerState#ledger_state.clients)),
        kill_client(ClientPid)
    end.

client_sends_a_letter_to_the_server_and_gets_its_changes_accepted(ServerPid) ->
    fun() ->
        ?assertEqual(true, is_process_alive(ServerPid)),
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

client_works_with_two_separate_changes(ServerPid) ->
    fun() ->
        ?assertEqual(true, is_process_alive(ServerPid)),
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

client_works_with_two_changes_one_after_another(ServerPid) ->
    fun() ->
        ?assertEqual(true, is_process_alive(ServerPid)),
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


% util functions
spawn_client() ->
    {ok, ClientPid} = client_editor:start_link(fun initializeNop/1, fun repaintNop/1),
    ClientPid.

kill_client(Pid) ->
    process_flag(trap_exit, true),
    exit(Pid, kill). %% brutal kill!

initializeNop(_) -> {ok, {10, 10}}.
repaintNop(_) -> ok.

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
