%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2016 16:35
%%%-------------------------------------------------------------------
-author("nietaki").



-record(local_state, {
    changes = [],
    cursor_position = 0,
    resulting_text = ""
}).

-record(client_state, {
    text,
    cursor_position,
    %display stuff
    display_repaint_fun,
    display_yx
}).

