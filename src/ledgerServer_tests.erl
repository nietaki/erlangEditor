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

foo_server_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun server_is_alive/1,
        fun server_registers_a_client/1,
        fun server_registers_a_client_andGetsChangesSubmitted/1,
        fun server_registers_a_client_andGetsTwoChangesSubmitted/1
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
        
        ledgerServer:register(),
        {state, 0, [], ClientPids, []} = ledgerServer:debug_get_state(),
        ?assert(maps:is_key(self(), ClientPids)),
        ?assertEqual(0, maps:get(self(), ClientPids))
    end.

server_registers_a_client_andGetsChangesSubmitted(_Pid) ->
    fun() ->
        ledgerServer:register(),
        ledgerServer:submit_local_changes(self(), 0, [{insert_char, 0, $x}]),
        {state, 1, "x", _ClientPids, [{insert_char, 0, $x}]} = ledgerServer:debug_get_state(),
        expect_cast({local_changes_accepted,0,1}) 
    end.

server_registers_a_client_andGetsTwoChangesSubmitted(_Pid) ->
    fun() ->
        ledgerServer:register(),
        ledgerServer:submit_local_changes(self(), 0, [{insert_char, 0, $x}, {insert_char, 1, $y}]),
        {state, 2, "xy", _ClientPids, [{insert_char, 0, $x}, {insert_char, 1, $y}]} = ledgerServer:debug_get_state(),
        expect_cast({local_changes_accepted,0,2}) 
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
    ?debugMsg("setup"),
    process_flag(trap_exit, true),
    {ok, Pid} = ledgerServer:start_link(),
    Pid.

cleanup(Pid) ->
    ?debugMsg("cleanup"),
    lib:flush_receive(),
    exit(Pid, kill), %% brutal kill!
    ?assertEqual(false, is_process_alive(Pid)).