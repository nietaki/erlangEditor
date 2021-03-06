%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2016 21:55
%%%-------------------------------------------------------------------
-module(client_editor).
-author("nietaki").

-behaviour(gen_server).
-include("../deps/cecho/include/cecho.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("client_editor.hrl").

%% API
-export([start_link/0, start_link/2, start_link/3]).
-export([send_char/2, send_keystroke/2]).
-export([debug_get_state/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(ceKEY_BACKSPACE, 127).
-define(ceKEY_BACKSPACE2, 8).

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
    start_link(fun cecho_display:initialize/1, fun cecho_display:repaint/2).

start_link(InitFun, RepaintFun) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [InitFun, RepaintFun], []).

start_link(InitFun, RepaintFun, ArbitraryName) ->
    gen_server:start_link({local, ArbitraryName}, ?MODULE, [InitFun, RepaintFun], []).
    

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
    {ok, State :: #client_state{}} | {ok, State :: #client_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([DisplayInitFun, DisplayRepaintFun]) ->
    cluster_utils:join_server_cluster(),
    Name = random_name(),
    RegisterResult = ledgerServer:register(Name),
    case RegisterResult of
        #ledger_head_state{head_id = _HeadId, head_text = Text} = LedgerHeadState ->
            {ok, DisplayYX} = DisplayInitFun(self()),
            LocalState = #local_state{ cursor_position = length(Text), resulting_text = Text},
            State = #client_state{ledger_head_state = LedgerHeadState, local_state = LocalState, display_repaint_fun = DisplayRepaintFun, display_yx = DisplayYX, cursor_position_throttling_state = utils:empty_throttling_state()},
            repaint(State),
            {ok, State};
        SomethingElse ->
            io:format("could not register~n"),
            hard_exit(),
            {stop, SomethingElse}
    end. 

hard_exit() ->
    application:stop(cecho),
    application:stop(erlangEditor),
    erlang:halt().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
        State :: #client_state{}) ->
    {reply, Reply :: term(), NewState :: #client_state{}} |
    {reply, Reply :: term(), NewState :: #client_state{}, timeout() | hibernate} |
    {noreply, NewState :: #client_state{}} |
    {noreply, NewState :: #client_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #client_state{}} |
    {stop, Reason :: term(), NewState :: #client_state{}}).
handle_call(get_state, _From, State) ->
    {reply, State, State}.

debug_get_state(Pid) -> gen_server:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #client_state{}) ->
    {noreply, NewState :: #client_state{}} |
    {noreply, NewState :: #client_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #client_state{}}).
handle_cast({ch, ?ceKEY_ESC}, State) -> 
    erlangEditor:stop(),
    {stop, user_pressed_esc, State};
handle_cast({ch, Ch}, #client_state{display_yx = YX} = State) ->
    EndState = handle_char(State, YX, Ch),
    repaint(EndState),
    submit_local_changes(EndState),
    EndStateWithThrottlingChanges = case is_movement_char(Ch) of
        true -> submit_seen_head_and_cursor_position_throttled(EndState);
        _ -> EndState 
    end,   
    {noreply, EndStateWithThrottlingChanges};
handle_cast({local_changes_accepted, OldId, ChangeCount}, State) ->
    #client_state{ledger_head_state = LedgerHeadState, local_state = _LocalState} = State,
    LastSeenLedgerHead = LedgerHeadState#ledger_head_state.head_id, 
    NewState = if
        OldId =< LastSeenLedgerHead ->
            merge_accepted_local_changes(State, ChangeCount - (LastSeenLedgerHead - OldId));
        true ->
            error(got_confirmation_of_too_new_changes)
    end,
    submit_local_changes(NewState),
    {noreply, NewState};
handle_cast({ledger_changed, BaseHeadId, ChangesSinceBaseHeadId}, State) when BaseHeadId =< State#client_state.ledger_head_state#ledger_head_state.head_id ->
    LastHeadIdSeenByTheClient = State#client_state.ledger_head_state#ledger_head_state.head_id,
    CountOfChangesToDrop = LastHeadIdSeenByTheClient - BaseHeadId,
    {_DiscardedChanges, RelevantChanges} = lists:split(CountOfChangesToDrop, ChangesSinceBaseHeadId),
    FinalState = handle_ledger_change(RelevantChanges, State),
    repaint(FinalState),
    case FinalState#client_state.local_state#local_state.changes of
        [] -> submit_seen_head_and_cursor_position(FinalState);
        _ -> ok % if there are some local changes, we will be submitting them anyways, notifying the server of both the seen head id and cursor position
    end,
    {noreply, FinalState};
handle_cast({ledger_changed, _BaseHeadId, _ChangesSinceBaseHeadId} = Request, State) ->
    error({got_ledger_changed_message_for_a_wrong_base_head_id, Request}),
    {noreply, State};
handle_cast({cursor_positions_at_head, PositionsMap, HeadId}, #client_state{cursor_positions = CursorPositions, ledger_head_state = LedgerHeadState} = State) ->
    if
        LedgerHeadState#ledger_head_state.head_id =:= HeadId ->
            NewCursorPositions = maps:merge(CursorPositions, PositionsMap),
            NewState = State#client_state{cursor_positions = NewCursorPositions},
            repaint(NewState),
            {noreply, NewState};
        true -> {noreply, State}
    end;
handle_cast({client_disconnected, Pid}, #client_state{cursor_positions = CursorPositions} = State) ->
    CursorPositionsWithoutDisconnectedClient = maps:without([Pid], CursorPositions),
    NewState = State#client_state{cursor_positions = CursorPositionsWithoutDisconnectedClient},
    repaint(NewState),
    {noreply, NewState};
handle_cast(Request, State) ->
    error({unrecognized_cast, Request}),
    {noreply, State}.

send_char(Pid, Char) when is_integer(Char) ->
    gen_server:cast(Pid, {ch, Char}).

send_keystroke(Pid, down) -> send_char(Pid, ?ceKEY_DOWN);
send_keystroke(Pid, up) -> send_char(Pid, ?ceKEY_UP);
send_keystroke(Pid, left) -> send_char(Pid, ?ceKEY_LEFT);
send_keystroke(Pid, right) -> send_char(Pid, ?ceKEY_RIGHT);
send_keystroke(Pid, delete) -> send_char(Pid, ?ceKEY_DEL);
send_keystroke(Pid, backspace) -> send_char(Pid, ?ceKEY_BACKSPACE).

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
-spec(handle_info(Info :: timeout() | term(), State :: #client_state{}) ->
    {noreply, NewState :: #client_state{}} |
    {noreply, NewState :: #client_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #client_state{}}).
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
        State :: #client_state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #client_state{},
        Extra :: term()) ->
    {ok, NewState :: #client_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

cursor_position_throttling_config() -> #throttling_config{max_execution_count = 10, time_window = 10000}.

-spec(submit_seen_head_and_cursor_position_throttled(#client_state{}) -> #client_state{}).
submit_seen_head_and_cursor_position_throttled(#client_state{cursor_position_throttling_state = ThrottlingState} = State) -> 
    Self = self(),
    NewThrottlingState = utils:apply_throttled(fun () -> submit_seen_head_and_cursor_position(State, Self) end, cursor_position_throttling_config(), ThrottlingState),
    State#client_state{cursor_position_throttling_state = NewThrottlingState}.

submit_seen_head_and_cursor_position(State) ->
    submit_seen_head_and_cursor_position(State, self()).

submit_seen_head_and_cursor_position(State, ClientPid) ->
ledgerServer:submit_seen_head_revision_and_cursor_position(State#client_state.ledger_head_state#ledger_head_state.head_id, State#client_state.local_state#local_state.cursor_position, ClientPid).

repaint(ClientState) ->
    #client_state{display_repaint_fun = RepaintFun, local_state = LocalState, cursor_positions = CursorPositions} = ClientState,
    CursorPositionsWithoutMine = maps:filter(fun (K, _) -> K =/= self() end, CursorPositions),
    RepaintFun(LocalState, maps:values(CursorPositionsWithoutMine)).

handle_ledger_change(ChangesSinceBaseHeadId, State) ->
    LocalState = State#client_state.local_state,
    LedgerHeadState = State#client_state.ledger_head_state,
    BaseHeadId = LedgerHeadState#ledger_head_state.head_id,
    PreviousLedgerHeadText = LedgerHeadState#ledger_head_state.head_text,
    NewLedgerHeadText = stringOps:apply_changes(PreviousLedgerHeadText, ChangesSinceBaseHeadId),
    LocalChangesPlusCursorPosition = [{insert_char, LocalState#local_state.cursor_position, $*} | LocalState#local_state.changes],
    CursorPositionPlusRebasedReverseChronologicalLocalChanges = lists:reverse(stringOps:rebase_changes(ChangesSinceBaseHeadId, lists:reverse(LocalChangesPlusCursorPosition))),
    NewLedgerHeadState = LedgerHeadState#ledger_head_state{head_id = BaseHeadId + length(ChangesSinceBaseHeadId), head_text = NewLedgerHeadText},
    [{insert_char, NewCursorPosition, $*}| ReverseChronologicalLocalChanges] = CursorPositionPlusRebasedReverseChronologicalLocalChanges,
    NewResultingText = stringOps:apply_changes(NewLedgerHeadText, lists:reverse(ReverseChronologicalLocalChanges)),
    NewLocalState = LocalState#local_state{changes = ReverseChronologicalLocalChanges, cursor_position = NewCursorPosition, resulting_text = NewResultingText},
    FinalState = State#client_state{local_state = NewLocalState, ledger_head_state = NewLedgerHeadState}, 
    
    submit_local_changes(FinalState),
    FinalState.

handle_char(ClientState, YX, Ch) ->
    LocalState = ClientState#client_state.local_state,
    CursorPosition = LocalState#local_state.cursor_position,
    NewLocalState = case Ch of
        Printable when Printable >= 20, Printable =< $~ -> insert_character(LocalState, Printable, CursorPosition);
        ?ceKEY_DOWN -> LocalState#local_state{cursor_position = get_new_position(CursorPosition, YX, down)};
        ?ceKEY_UP -> LocalState#local_state{cursor_position = get_new_position(CursorPosition, YX, up)};
        ?ceKEY_LEFT -> LocalState#local_state{cursor_position = get_new_position(CursorPosition, YX, left)};
        ?ceKEY_RIGHT -> LocalState#local_state{cursor_position = get_new_position(CursorPosition, YX, right)};
        ?ceKEY_DEL -> delete_character(LocalState, CursorPosition);
        ?ceKEY_BACKSPACE -> delete_character(LocalState, CursorPosition - 1);
        ?ceKEY_BACKSPACE2-> delete_character(LocalState, CursorPosition - 1);
        _ -> LocalState 
        %SomethingElse -> error(SomethingElse) 
    end,
    ClientState#client_state{local_state = fixup_state_position(NewLocalState)}.

is_movement_char(?ceKEY_DOWN) -> true;
is_movement_char(?ceKEY_UP) -> true;
is_movement_char(?ceKEY_LEFT) -> true;
is_movement_char(?ceKEY_RIGHT) -> true;
is_movement_char(_) -> false.

fixup_state_position(#local_state{resulting_text = Text, cursor_position = Position}= LocalState) ->
    LocalState#local_state{resulting_text = Text, cursor_position = max(min(Position, string:len(Text)), 0)}.

insert_character(#local_state{resulting_text = Text, cursor_position = CurrentPosition, changes = Changes}= LocalState, Character, Position) -> 
    NewCursorPosition = if
        Position =< CurrentPosition-> CurrentPosition + 1;
        true -> CurrentPosition
    end,
    LocalState#local_state{resulting_text = stringOps:insert_char(Text, Character, Position), 
        cursor_position = NewCursorPosition, 
        changes = [{insert_char, Position, Character}| Changes]}.

delete_character(#local_state{resulting_text = Text, cursor_position = CurrentPosition, changes = Changes} = LocalState, PositionToDelete) ->
    if
        PositionToDelete < 0 -> LocalState;
        PositionToDelete >= length(Text) -> LocalState;
        true ->
            NewCursorPosition = if
                PositionToDelete < CurrentPosition -> CurrentPosition - 1;
                true -> CurrentPosition
            end, 
            LocalState#local_state{resulting_text = stringOps:delete_char(Text, PositionToDelete), 
            cursor_position = NewCursorPosition,
            changes = [{delete_char, PositionToDelete} | Changes]}
    end.

get_new_position(OriginalPosition, {ConsoleHeight, ConsoleWidth}, Direction) ->
    NewPosition = case Direction of
        down -> OriginalPosition + ConsoleWidth;
        up -> OriginalPosition - ConsoleWidth;
        left -> OriginalPosition - 1;
        right -> OriginalPosition + 1
        %_ -> OriginalPosition 
    end,
    min(max(0, NewPosition), ConsoleHeight * ConsoleWidth - 1).

submit_local_changes(#client_state{ledger_head_state = LedgerHeadState, local_state = LocalState} = _ClientState) ->
    LocalChanges = LocalState#local_state.changes,
    case LocalChanges of
        [] -> ok;
        NonEmpty ->
            OldestToNewestChanges = lists:reverse(NonEmpty),
            ledgerServer:submit_local_changes(self(), LedgerHeadState#ledger_head_state.head_id, OldestToNewestChanges),
            ok
    end.

merge_accepted_local_changes(State, ChangeCount) ->
    #client_state{ledger_head_state = LedgerHeadState, local_state = LocalState} = State,
    Changes = lists:reverse(LocalState#local_state.changes), %now the changes are old to new
    {ChangesAccepted, ChangesLeft} = lists:split(ChangeCount, Changes),
    NewHeadText = stringOps:apply_changes(LedgerHeadState#ledger_head_state.head_text, ChangesAccepted),
    NewLedgerHeadId = LedgerHeadState#ledger_head_state.head_id + ChangeCount,
    NewLedgerHeadState = LedgerHeadState#ledger_head_state{head_id = NewLedgerHeadId, head_text = NewHeadText},
    State#client_state{local_state = LocalState#local_state{changes = lists:reverse(ChangesLeft)}, ledger_head_state = NewLedgerHeadState}.

possible_names() ->
    ["Thomas", "James", "Jack", "Daniel", "Matthew", "Ryan", "Luke", "Samuel", "Jordan", "Adam", "Christopher", "Benjamin", "Joseph", "Liam", "William", "George", "Oliver", "Nathan", "Harry", "Kyle"].

random_name() ->
    Names = possible_names(),
    Idx = rand:uniform(length(Names)),
    lists:nth(Idx, Names).