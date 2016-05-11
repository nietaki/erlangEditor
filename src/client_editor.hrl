%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2016 16:35
%%%-------------------------------------------------------------------
-author("nietaki").

-record(client_state, {
    text,
    cursorPosition,
    display_repaint_fun,
    display_yx
}).

