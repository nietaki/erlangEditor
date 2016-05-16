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
-export([join_server_cluster/0, test_connecting_to_server/0]).

join_server_cluster() -> 
    ServerName = server@localhost,
    %io:format("pinging ~w~n", [ServerName]),
    net_adm:ping(ServerName), %this does the job if the cookies are setup correctly
    timer:sleep(200). % and this makes sure we're fully connected

test_connecting_to_server() ->
    join_server_cluster(),
    io:format("~w~n", [nodes()]),
    io:format("~w~n", [ledgerServer:debug_get_state()]),
    ok.


