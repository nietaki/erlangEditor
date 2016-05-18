%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2016 23:57
%%%-------------------------------------------------------------------
-module(utils_tests).
-author("nietaki").

-include_lib("eunit/include/eunit.hrl").
-include("utils.hrl").

%% API
-export([]).


time_window() -> 40.

test_throttling_config() -> #throttling_config{max_execution_count = 2, time_window = time_window()}.

apply_throttled_with_empty_state_applies_the_function_test() ->
    utils:apply_throttled(test_utils:send_ping_to_current_process(), test_throttling_config(), utils:empty_throttling_state()),
    test_utils:expect_ping_message().

apply_throttled_fills_up_the_whole_max_execution_count_test() ->
    State2 = utils:apply_throttled(test_utils:send_ping_to_current_process(), test_throttling_config(), utils:empty_throttling_state()),
    utils:apply_throttled(test_utils:send_ping_to_current_process(), test_throttling_config(), State2),
    test_utils:expect_ping_message(),
    test_utils:expect_ping_message(),
    test_utils:expect_no_ping_message().

triggering_the_throttled_function_more_times_than_the_max_execution_count_doesnt_execute_it_the_last_time_test() ->
    State2 = utils:apply_throttled(test_utils:send_to_current_process(1), test_throttling_config(), utils:empty_throttling_state()),
    State3 = utils:apply_throttled(test_utils:send_to_current_process(2), test_throttling_config(), State2),
    utils:apply_throttled(test_utils:send_to_current_process(3), test_throttling_config(), State3),
    test_utils:expect_message(1),
    test_utils:expect_message(2),
    test_utils:expect_no_message(3).

the_queue_does_empty_up_after_enough_time_test() ->
    State2 = utils:apply_throttled(test_utils:send_to_current_process(1), test_throttling_config(), utils:empty_throttling_state()),
    State3 = utils:apply_throttled(test_utils:send_to_current_process(2), test_throttling_config(), State2),
    timer:sleep(time_window() + 10),
    utils:apply_throttled(test_utils:send_to_current_process(3), test_throttling_config(), State3),
    test_utils:expect_message(1),
    test_utils:expect_message(2),
    test_utils:expect_message(3).


