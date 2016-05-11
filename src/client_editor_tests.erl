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
-include_lib("ledgerServer.hrl").

client_editor_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun client_connects_to_server/1
    ]}.

client_connects_to_server(ServerPid) ->
    fun() ->
        ?assertEqual(true, is_process_alive(ServerPid)),
        ClientPid = spawn_client(),
        ServerState = ledgerServer:debug_get_state(),
        ?assertEqual(1, maps:size(ServerState#ledger_state.clients)),
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
