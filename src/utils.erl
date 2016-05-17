%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2016 22:42
%%%-------------------------------------------------------------------
-module(utils).
-author("nietaki").

-include("utils.hrl").

%% API
-export([apply_throttled/3, empty_throttling_state/0]).

-spec(apply_throttled(Fun :: fun(), ThrottlingConfig :: #throttling_config{}, ThrottlingState :: #throttling_state{}) -> #throttling_state{}).
apply_throttled(Fun, 
        #throttling_config{max_execution_count = MaxExecutionCount, time_window = TimeWindow}, 
        #throttling_state{timer_ref = TimerRef, execution_times = ExecutionTimes} = ThrottlingState) ->
    % TODO cancel the timer here 
    % TODO handle the queue here
    Fun(),
    % TODO set the timer up here
    ThrottlingState.

empty_throttling_state() -> #throttling_state{timer_ref = none, execution_times = queue:new()}.


%safe_cancel_timer(TimerRef) -> error(todo).