%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2016 23:33
%%%-------------------------------------------------------------------
-module(test_utils).
-author("nietaki").

%% API
-export([expect_cast/1, expect_no_cast/0, expect_cast_of_type/1, expect_ping_message/0]).
-export([send_ping_to/1, send_ping_to_current_process/0, expect_no_ping_message/0]).

-include_lib("eunit/include/eunit.hrl").

expect_cast_of_type(CastType) ->
receive
        {'$gen_cast', ReceivedMessage}->
            ?assertEqual(CastType, element(1, ReceivedMessage))
    after 10 ->
        ?assert(no_cast_message_received)
    end.

expect_cast(CastMessage) ->
    receive
        {'$gen_cast', ReceivedMessage}->
            ?assertEqual(CastMessage, ReceivedMessage)
    after 10 ->
        ?assert(no_cast_message_received)
    end.

expect_no_cast() ->
    receive
        {'$gen_cast', _ReceivedMessage} -> ?assert(false)
    after 10 ->
        ?assert(true)
    end.

send_ping_to_current_process() -> send_ping_to(self()).

send_ping_to(Pid) -> 
    fun() -> Pid ! ping end.

expect_no_ping_message() ->
    receive
        ping -> ?assert(ping_message_received)
    after 10 ->
        ?assert(true)
    end.

expect_ping_message() ->
    receive
        ping -> ?assert(true)
    after 10 ->
        ?assert(ping_message_not_received)
    end.
