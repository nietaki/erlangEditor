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
-export([start/0]).

-include("../deps/cecho/include/cecho.hrl").

start() -> 
    application:ensure_all_started(erlangEditor),
    application:start(erlangEditor).
