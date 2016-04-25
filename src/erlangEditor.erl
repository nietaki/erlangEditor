%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2016 20:22
%%%-------------------------------------------------------------------
-module(erlangEditor).
-author("nietaki").

%% API
%-export([start/0, cecho_test/0]).
-export([start/0]).

-include("../deps/cecho/include/cecho.hrl").



start() -> 
    application:ensure_all_started(erlangEditor),
    application:start(erlangEditor).

%% NOTE, ANYTHING BELOW ISN'T USED ANYMORE, JUST KEPT HERE FOR REFERENCE %%
cecho_test() ->
    ok = cecho:cbreak(),
    ok = cecho:noecho(),
    ok = cecho:keypad(?ceSTDSCR, true),
    ok = cecho:curs_set(?ceCURS_NORMAL),
    %% Write initial string...
    cecho_loop(0, 0).

cecho_loop(Y, X) ->
    {MaxY, MaxX} = cecho:getmaxyx(),
    ok = cecho:mvaddstr(0, 0, "Hello World!"),
    ok = cecho:move(Y, X),
    %{CurY, CurX} = cecho:getyx(),
    ok = cecho:refresh(),
    Ch = cecho:getch(),
    {YDiff, XDiff} = get_offset(Ch),
    cecho:mvaddch(10, 10, Ch),
    cecho_loop((Y + YDiff + MaxY) rem MaxY, (X + XDiff + MaxX) rem MaxX). 

get_offset(Ch) ->
    case Ch of
        ?ceKEY_DOWN -> {1, 0};
        ?ceKEY_UP -> {-1, 0};
        ?ceKEY_LEFT -> {0, -1};
        ?ceKEY_RIGHT -> {0, 1};
        _ -> {0, 0}
    end.