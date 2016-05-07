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
-export([start/0, stop/0]).

-include("../deps/cecho/include/cecho.hrl").

start() -> 
    cluster_utils:join_server_cluster(),
    application:ensure_all_started(erlangEditor),
    application:start(erlangEditor).

stop() ->
    application:stop(cecho),
    application:stop(erlangEditor),
    init:stop(),
    erlang:halt(). % ok, I don't really know how to exit an Erlang app elegantly right now