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
-include_lib("eunit/include/eunit.hrl").

%% API
-export([apply_throttled/3, empty_throttling_state/0]).

empty_throttling_state() -> #throttling_state{timer_ref = none, execution_times = queue:new()}.

-spec(apply_throttled(Fun :: fun(), ThrottlingConfig :: #throttling_config{}, ThrottlingState :: #throttling_state{}) -> #throttling_state{}).
apply_throttled(Fun, 
        #throttling_config{max_execution_count = MaxExecutionCount, time_window = TimeWindow}, 
        #throttling_state{timer_ref = TimerRef, execution_times = ExecutionTimes} = ThrottlingState) ->
    % TODO cancel the timer here 
    Now = erlang:timestamp(),
    PrunedExecutionTimes = remove_older_execution_times(Now, TimeWindow, ExecutionTimes),
    NewExecutionTimes = case queue:len(PrunedExecutionTimes) of
        LessThanMax when LessThanMax < MaxExecutionCount ->
            Fun(),
            queue:in(Now, PrunedExecutionTimes);
        _ -> 
           PrunedExecutionTimes
    end,
    % TODO set the timer up here
    ThrottlingState#throttling_state{execution_times = NewExecutionTimes}.

% MaxTimeDiff is in milliseconds
remove_older_execution_times(GivenTime, MaxTimeDiff, Queue) ->
    queue:filter(fun(T) -> timer:now_diff(GivenTime, T) < (MaxTimeDiff * 1000) end, Queue).

remove_older_execution_times_test() -> 
    Q = queue:from_list([{0,1,0}, {0,3,0}, {0,5,0}]),
    Q2 = remove_older_execution_times({0,4,0}, 2000, Q),
    ?assertEqual([{0,3,0}, {0,5,0}], queue:to_list(Q2)).


%safe_cancel_timer(TimerRef) -> error(todo).