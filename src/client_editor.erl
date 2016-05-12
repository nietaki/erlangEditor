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
-export([start_link/0, start_link/2]).
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
    start_link(fun cecho_display:initialize/1, fun cecho_display:repaint/1).

start_link(InitFun, RepaintFun) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [InitFun, RepaintFun], []).


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
            State = #client_state{ledger_head_state = LedgerHeadState, local_state = LocalState, display_repaint_fun = DisplayRepaintFun, display_yx = DisplayYX},
            Repaint = State#client_state.display_repaint_fun,
            ok = Repaint(LocalState),
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
handle_cast({ch, Ch}, #client_state{local_state = LocalState, display_yx = YX} = State) ->
    NewLocalState = handle_char(LocalState, YX, Ch),
    RepaintFun = State#client_state.display_repaint_fun,
    RepaintFun(NewLocalState),
    EndState = State#client_state{local_state = NewLocalState},
    submit_local_changes(EndState),
    {noreply, EndState};
handle_cast({local_changes_accepted, OldId, ChangeCount}, State) ->
    #client_state{ledger_head_state = LedgerHeadState, local_state = LocalState} = State,
    LastSeenLedgerHead = LedgerHeadState#ledger_head_state.head_id, 
    NewState = if
        OldId =< LastSeenLedgerHead ->
            merge_accepted_local_changes(State, ChangeCount - (LastSeenLedgerHead - OldId));
        true ->
            error(got_confirmation_of_too_new_changes)
    end,
    submit_local_changes(NewState),
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

-spec(handle_char(LocalState :: #local_state{}, YX :: {integer(), integer()}, Ch ::char()) -> #client_state{}).
handle_char(LocalState, YX, Ch) ->
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
    EndState = fixup_state_position(NewLocalState),
    EndState.
  
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

delete_character(#local_state{resulting_text = Text, cursor_position = CurrentPosition} = LocalState, PositionToDelete) ->
    NewCursorPosition = if
        PositionToDelete < CurrentPosition -> CurrentPosition - 1;
        true -> CurrentPosition
    end, 
    LocalState#local_state{resulting_text = stringOps:delete_char(Text, PositionToDelete), 
        cursor_position = NewCursorPosition,
        changes = [{delete_char, PositionToDelete}]}.

get_new_position(OriginalPosition, {ConsoleHeight, ConsoleWidth}, Direction) ->
    NewPosition = case Direction of
        down -> OriginalPosition + ConsoleWidth;
        up -> OriginalPosition - ConsoleWidth;
        left -> OriginalPosition - 1;
        right -> OriginalPosition + 1
        %_ -> OriginalPosition 
    end,
    min(max(0, NewPosition), ConsoleHeight * ConsoleWidth - 1).

submit_local_changes(#client_state{ledger_head_state = LedgerHeadState, local_state = LocalState}) ->
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
    {ok, NewHeadText} = stringOps:apply_changes(LedgerHeadState#ledger_head_state.head_text, ChangesAccepted),
    NewLedgerHeadId = LedgerHeadState#ledger_head_state.head_id + ChangeCount,
    NewLedgerHeadState = LedgerHeadState#ledger_head_state{head_id = NewLedgerHeadId, head_text = NewHeadText},
    State#client_state{local_state = LocalState#local_state{changes = lists:reverse(ChangesLeft)}, ledger_head_state = NewLedgerHeadState}.

possible_names() ->
    ["Thomas", "James", "Jack", "Daniel", "Matthew", "Ryan", "Luke", "Samuel", "Jordan", "Adam", "Christopher", "Benjamin", "Joseph", "Liam", "William", "George", "Oliver", "Nathan", "Harry", "Kyle"].

random_name() ->
    Names = possible_names(),
    Idx = random:uniform(length(Names)),
    lists:nth(Idx, Names).