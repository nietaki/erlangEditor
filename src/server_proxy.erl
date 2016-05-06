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

%% API
-export([start/1, kill/1, call/2, cast/2, receive_a_cast_message/1]).
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
            end
    end,
    loop(ServerPid).

call(Proxy, Message) ->
    Proxy ! {call, self(), Message},
    return_received_message().

cast(Proxy, Message) ->
    Proxy ! {cast, self(), Message},
    ok.

kill(Proxy) ->
    exit(Proxy, kill).

receive_a_cast_message(Proxy) ->
    Proxy ! {get_cast_message, self()},
    return_received_message().

return_received_message() ->
    receive
        {proxy_response, Message} -> Message
    after 10 ->
        no_message_received_by_server_proxy
    end.
