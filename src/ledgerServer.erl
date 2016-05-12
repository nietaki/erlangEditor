%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 26. Apr 2016 22:06
%%%-------------------------------------------------------------------
-module(ledgerServer).
-author("nietaki").

-behaviour(gen_server).

%% API
-export([start_link/0, register/1, submit_local_changes/3]).

%% debug API
-export([debug_get_state/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ledgerServer.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    ServerRef = server_ref(),
    debug_msg("starting server as ~p", [ServerRef]),
    gen_server:start_link(ServerRef, ?MODULE, [], []).

server_ref() -> {global, ?MODULE}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #ledger_state{}} | {ok, State :: #ledger_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    debug_msg("ledger_server started", []),
    {ok, #ledger_state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
        State :: #ledger_state{}) ->
    {reply, Reply :: term(), NewState :: #ledger_state{}} |
    {reply, Reply :: term(), NewState :: #ledger_state{}, timeout() | hibernate} |
    {noreply, NewState :: #ledger_state{}} |
    {noreply, NewState :: #ledger_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #ledger_state{}} |
    {stop, Reason :: term(), NewState :: #ledger_state{}}).
handle_call({register, Username}, From, State) ->
    #ledger_state{head_id = HeadId, clients = Clients} = State,
    NewClientState = #client_info{username = Username, last_seen_head = HeadId},
    NewClients = Clients#{get_client_pid(From) => NewClientState},
    debug_msg("client ~s joined as pid ~p", [Username, From]),
    {reply, get_ledger_state_message(State), State#ledger_state{clients = NewClients}};
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

register(Username) ->
    gen_server:call(server_ref(), {register, Username}, 500).

debug_get_state() ->
    gen_server:call(server_ref(), get_state).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #ledger_state{}) ->
    {noreply, NewState :: #ledger_state{}} |
    {noreply, NewState :: #ledger_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #ledger_state{}}).
handle_cast({submit_local_changes, Sender, BaseHeadId, NewChanges}=Message, State) ->
    #ledger_state{head_id = StateHeadId} = State,
    case BaseHeadId of
        StateHeadId -> 
            case lists:all(fun stringOps:is_a_change/1, NewChanges) of
                false ->
                    debug_msg("received broken changes", []),
                    {noreply, State}; 
                true ->
                    debug_msg("received changes ~p", [Message]),
                    case stringOps:apply_changes(State#ledger_state.head_text, NewChanges) of
                        {ok, NewText} -> 
                            #ledger_state{head_id = OldId, changes = OldChanges, clients = ClientsMap} = State,
                            % updating Sender's last seen head_id
                            NewId = OldId + length(NewChanges),
                            #{Sender := OldClientInfo} = ClientsMap,
                            NewClientInfo = OldClientInfo#client_info{last_seen_head = NewId},
                            NewClientsMap = ClientsMap#{Sender := NewClientInfo},
                            
                            gen_server:cast(Sender, {local_changes_accepted, OldId, length(NewChanges)}),
                            StateWithAppliedChanges = State#ledger_state{head_id = NewId, clients = NewClientsMap, head_text = NewText, changes = OldChanges ++ NewChanges},
                            cast_changes_to_all_clients_except_for(StateWithAppliedChanges, Sender),
                            debug_msg("changes applied", []),
                            debug_msg("new state: ~p", [StateWithAppliedChanges]),
                            {noreply, StateWithAppliedChanges};
                        _ -> 
                            debug_msg("couldn't apply incorrect changes", []),
                            {noreply, State} 
                    end
            end; 
        Older when Older < StateHeadId ->
            debug_msg("received old changes: ~p", [Message]),
            LSH = get_last_seen_head_id_for_client(State, Sender),
            if
                LSH > Older -> 
                    % client has seen, and probably created, changes newer than these, let's not resend them to them
                    ok;
                true -> 
                    cast_changes_to_client(State, Sender)
            end,
            {noreply, State};
        Newer when Newer > StateHeadId ->
            debug_msg("can't apply changes based on head_id from the future: ~p~n", [Message]),
            error(received_head_id_too_new)
    end;
handle_cast(_Request, State) ->
    {noreply, State}.

submit_local_changes(Pid, BaseHeadId, NewChanges) ->
    gen_server:cast(server_ref(), {submit_local_changes, Pid, BaseHeadId, NewChanges}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #ledger_state{}) ->
    {noreply, NewState :: #ledger_state{}} |
    {noreply, NewState :: #ledger_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #ledger_state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
        State :: #ledger_state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #ledger_state{},
        Extra :: term()) ->
    {ok, NewState :: #ledger_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_ledger_state_message(#ledger_state{head_id = HeadId, head_text = Text}) ->
    #ledger_head_state{head_id = HeadId, head_text = Text}.

get_client_pid({ClientPid, _Tag}) -> ClientPid.

% yes, the casting changes to clients could be optimised by using the map more directly
cast_changes_to_all_clients_except_for(State, ClientPid) ->
    ClientList = maps:keys(State#ledger_state.clients),
    ClientListFiltered = lists:filter(fun(C) -> C =/= ClientPid end, ClientList),
    lists:map(fun(C) -> cast_changes_to_client(State, C) end, ClientListFiltered).

cast_changes_to_client(State, ClientPid) ->
    gen_server:cast(ClientPid, get_changes_for_client(State, ClientPid)),
    ok.

get_last_seen_head_id_for_client(State, ClientPid) ->
    ClientInfo = maps:get(ClientPid, State#ledger_state.clients),
    ClientInfo#client_info.last_seen_head.

get_changes_for_client(State, ClientPid) ->
    LSH = get_last_seen_head_id_for_client(State, ClientPid),
    {ledger_changed, LSH, get_changes_since(LSH, State)}.
    
get_changes_since(LastSeenHeadId, #ledger_state{head_id = HeadId, changes = Changes}) when LastSeenHeadId =< HeadId ->
    DesiredCount = HeadId - LastSeenHeadId,
    lists:nthtail(length(Changes) - DesiredCount, Changes).

get_changes_since_test_() ->
    [
        ?_assertEqual([], get_changes_since(0, #ledger_state{head_id = 0, changes = []})),
        ?_assertEqual([], get_changes_since(2, #ledger_state{head_id = 2, changes = [foo, bar]})),
        ?_assertEqual([bar], get_changes_since(1, #ledger_state{head_id = 2, changes = [foo, bar]})),
        ?_assertEqual([foo, bar], get_changes_since(0, #ledger_state{head_id = 2, changes = [foo, bar]})),
        ?_assertError(function_clause, get_changes_since(10000, #ledger_state{head_id = 1, changes = [foo]}))
    ].

debug_msg(Format, Arguments) ->
    io:format(Format, Arguments),
    io:nl().
