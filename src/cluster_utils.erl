%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2016 23:54
%%%-------------------------------------------------------------------
-module(cluster_utils).
-author("nietaki").

%% API
-export([join_server_cluster/0]).

join_server_cluster() -> 
    ServerName = server@localhost,
    io:format("pinging ~w", [ServerName]),
    net_adm:ping(ServerName). %this does the job if the cookies are setup correctly
