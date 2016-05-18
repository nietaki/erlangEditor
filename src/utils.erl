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
-export([apply_throttled/3, empty_throttling_state/0, function_proxy/1]).

empty_throttling_state() -> #throttling_state{timer_ref = none, execution_times = queue:new()}.

%% @doc
%% apply_throttled allows you to smartly throttle executions of a Fun, in a situation where any of the Fun executions 
%% can be skipped and the later executions are more important than the earlier ones. 
%%
%% Deliberately choosing the throttling_config allows you to tolerate short bursts while keeping the overall rate limited.
%%
%% For example let's say we're limiting the rate of sending cursor location updates. Setting the
%% #throttling_config.time_window to 10000 and the #throttling_config.max_execution_count to 20 will allow the user to
%% make move the cursor quickly for up to 20 times and have its location instantly broadcasted, while keeping the overall 
%% rate of updates down to 2/second.
%%
%% Please note, that the last Fun passed to apply_throttled will always eventually get executed even if the calling rate
%% is currently over the limit - the Fun gets scheduled to be applied when the time window allows for it. This allows 
%% for the end state to always be as expected.
-spec(apply_throttled(Fun :: fun(), ThrottlingConfig :: #throttling_config{}, ThrottlingState :: #throttling_state{}) -> #throttling_state{}).
apply_throttled(Fun, 
        #throttling_config{max_execution_count = MaxExecutionCount, time_window = TimeWindow}, 
        #throttling_state{timer_ref = TimerRef, execution_times = ExecutionTimes} = ThrottlingState) ->
    timer:cancel(TimerRef), % we might consider checking if TimerRef isn't 'none' and only then cancelling it and setting to none. I will leave this like this for clarity now
    Now = erlang:timestamp(),
    PrunedExecutionTimes = remove_older_execution_times(Now, TimeWindow, ExecutionTimes),
    {NewTimerRef, NewExecutionTimes} = case queue:len(PrunedExecutionTimes) of
        LessThanMax when LessThanMax < MaxExecutionCount ->
            Fun(),
            {TimerRef, queue:in(Now, PrunedExecutionTimes)};
        _Max -> 
            {value, EarliestTimeInQueue} = queue:peek(PrunedExecutionTimes),
            CurrentTimeDiff = time_diff_in_ms(Now, EarliestTimeInQueue),
            RemainingTime = max(0, TimeWindow - CurrentTimeDiff),
            {ok, CreatedTimerRef} = timer:apply_after(RemainingTime, ?MODULE, function_proxy, [Fun]),
            {CreatedTimerRef, PrunedExecutionTimes}
    end,
    ThrottlingState#throttling_state{execution_times = NewExecutionTimes, timer_ref = NewTimerRef}.

time_diff_in_ms(Later, Earlier) -> timer:now_diff(Later, Earlier) div 1000.

function_proxy(Fun) -> Fun().

% MaxTimeDiff is in milliseconds
remove_older_execution_times(GivenTime, MaxTimeDiff, Queue) ->
    queue:filter(fun(T) -> time_diff_in_ms(GivenTime, T) =< MaxTimeDiff end, Queue).

remove_older_execution_times_test() -> 
    Q = queue:from_list([{0,1,0}, {0,3,0}, {0,5,0}]),
    Q2 = remove_older_execution_times({0,4,0}, 2000, Q),
    ?assertEqual([{0,3,0}, {0,5,0}], queue:to_list(Q2)).
