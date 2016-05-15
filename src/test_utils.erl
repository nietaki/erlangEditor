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
-export([expect_cast/1, expect_no_cast/0, expect_cast_of_type/1]).

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
