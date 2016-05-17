%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2016 22:42
%%%-------------------------------------------------------------------
-author("nietaki").

% represents a configuration of how the execution should be throttled - "how frequent is too frequent"
-record(throttling_config, {
    % How many executions are allowed per time period. If in a rolling window there are more than `execution_count` attempts
    % at running the throttled Fun, they will be delayed until after the period has run out.
    max_execution_count = 2,
    
    % the time window in ms
    time_window = 1000
}).

% keeps track of when the executions were actually performed
-record(throttling_state, {
    % reference to the timer containing the pending execution
    timer_ref = none,
    
    % the queue of the times the execution has actually been performed
    execution_times = queue:new()
}).


