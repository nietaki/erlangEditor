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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    {reply, get_ledger_state_message(State), State#ledger_state{clients = NewClients}};
handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

register(Username) ->
    gen_server:call(?MODULE, {register, Username}).

debug_get_state() ->
    gen_server:call(?MODULE, get_state).

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
handle_cast({submit_local_changes, Pid, BaseHeadId, NewChanges}=Message, State) ->
    #ledger_state{head_id = StateHeadId} = State,
    case BaseHeadId of
        StateHeadId -> 
            case lists:all(fun is_a_change/1, NewChanges) of
                false -> {noreply, State}; %changes were broken
                true ->
                    case apply_changes(State#ledger_state.head_text, NewChanges) of
                        {ok, NewText} -> 
                            OldId = State#ledger_state.head_id,
                            NewId = OldId + length(NewChanges),
                            OldChanges = State#ledger_state.changes,
                            ClientsMap = State#ledger_state.clients,
                            NewClientsMap = ClientsMap#{Pid := NewId},
                            gen_server:cast(Pid, {local_changes_accepted, OldId, length(NewChanges)}),
                            {noreply, State#ledger_state{head_id = NewId, clients = NewClientsMap, head_text = NewText, changes = OldChanges ++ NewChanges}};
                        _ -> 
                            io:format("couldn't apply incorrect changes: ~w~n", Message),
                            {noreply, State} %changes couldn't be applied
                    end
            end; 
        Older when Older < StateHeadId -> {noreply, State}; %TODO send the changes since older
        Newer when Newer > StateHeadId ->
            io:format("can't apply changes based on head_id from the future: ~w~n", Message),
            error(received_head_id_too_new)
    end;
handle_cast(_Request, State) ->
    {noreply, State}.

submit_local_changes(Pid, BaseHeadId, NewChanges) ->
    gen_server:cast(?MODULE, {submit_local_changes, Pid, BaseHeadId, NewChanges}).

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
    {ledger_head_state, HeadId, Text}.

get_client_pid({ClientPid, _Tag}) -> ClientPid.

is_printable(Char) -> is_integer(Char) and (Char >= 32) and (Char =< 126).

%changes {delete_char, Pos}, {insert_char, Pos, Char}
is_a_change({delete_char, Pos}) when is_integer(Pos) -> true;
is_a_change({insert_char, Pos, Char}) when is_integer(Pos), is_integer(Char), Char >= 32, Char =< 126 -> true;
is_a_change(_) -> false.

apply_change({delete_char, Pos}, Text) ->
    PosIsCorrect = (Pos >= 0) and (Pos < string:len(Text)),
    if
        PosIsCorrect -> {ok, stringOps:delete_char(Text, Pos)};
        true -> {fail, Text}
    end;
apply_change({insert_char, Pos, Char}, Text) ->
    IsProperChar = is_printable(Char),
    PosIsCorrect = (Pos >=0) and (Pos =< string:len(Text)),
    if
        IsProperChar, PosIsCorrect -> {ok, stringOps:insert_char(Text, Char, Pos)};
        true -> {fail, Text}
    end.

apply_changes(Text, []) -> {ok, Text};
apply_changes(Text, [H|T]) ->
    case apply_change(H, Text) of
        {ok, NewText} -> apply_changes(NewText, T);
        Else -> Else
    end.

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
