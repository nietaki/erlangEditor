%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2016 16:35
%%%-------------------------------------------------------------------
-author("nietaki").

-include("ledgerServer.hrl").

-record(local_state, {
    changes = [],
    cursor_position = 0,
    resulting_text = ""
}).

-record(client_state, {
    ledger_head_state = #ledger_head_state{},
    local_state = #local_state{},
    cursor_positions = #{},
    %display stuff
    display_repaint_fun,
    display_yx,
    %throttling stuff
    cursor_position_throttling_state = #throttling_state{}
}).

