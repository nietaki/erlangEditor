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
-export([start/0, cecho_test/0]).
-include("../deps/cecho/include/cecho.hrl").


start() -> 
    application:start(cecho),
    application:start(erlangEditor).

cecho_test() ->
    ok = cecho:cbreak(),
    ok = cecho:noecho(),
    ok = cecho:curs_set(?ceCURS_INVISIBLE),
    %% Write initial string...
    ok = cecho:mvaddstr(0, 0, "Hello World!"),
    ok = cecho:refresh(),
    cecho_test().
