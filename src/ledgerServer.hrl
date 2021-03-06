%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2016 23:37
%%%-------------------------------------------------------------------
-author("nietaki").

-include("utils.hrl").

% clients is a map of Pid to client_info record
-record(ledger_state, {head_id = 0, 
    head_text = "", 
    clients = #{}, 
    changes = [], 
    cursor_position_throttling_state = #throttling_state{}
}).

% sent to the clients
-record(ledger_head_state, {
    head_id = 0,
    head_text = ""
}).

-record(client_info, {
    username, 
    last_seen_head,
    cursor_position
}).
