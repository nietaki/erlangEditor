%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 06. May 2016 21:43
%%%-------------------------------------------------------------------
-module(server_proxy).
-author("nietaki").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start/1, kill/1, run_fun/2, call/2, cast/2, expect_no_cast/1, receive_a_cast_message/1]).
-export([loop/1]).

% returns a Pid to talk to
start(ServerPid) ->
    spawn(?MODULE, loop, [ServerPid]).


loop(ServerPid) ->
    receive
        {call, FromPid, Message} ->
            FromPid ! {proxy_response, gen_server:call(ServerPid, Message)};
        {cast, _FromPid, Message} ->
            gen_server:cast(ServerPid, Message);
        {get_cast_message, FromPid} ->
            receive
                {'$gen_cast', CastMessage} -> 
                    FromPid ! {proxy_response, CastMessage}
            end;
        {expect_no_cast, FromPid} ->
            receive
                {'$gen_cast', _CastMessage} -> 
                    FromPid ! {proxy_response, cast_received}
            after 10 ->
                FromPid ! {proxy_response, no_cast_received}
            end;
        {run_fun, FromPid, Fun} ->
            FromPid ! {proxy_response, Fun()}
    end,
    loop(ServerPid).

call(Proxy, Message) ->
    Proxy ! {call, self(), Message},
    receive_message().

cast(Proxy, Message) ->
    Proxy ! {cast, self(), Message},
    ok.

run_fun(Proxy, Fun) ->
    Proxy ! {run_fun, self(), Fun},
    receive_message().

kill(Proxy) ->
    exit(Proxy, kill).

receive_a_cast_message(Proxy) ->
    Proxy ! {get_cast_message, self()},
    receive_message().

expect_no_cast(Proxy) ->
    Proxy ! {expect_no_cast, self()},
    ?assertEqual(no_cast_received, receive_message()).

receive_message() ->
    receive
        {proxy_response, Message} -> Message
    after 10 ->
        ?assert(no_message_received)
    end.
