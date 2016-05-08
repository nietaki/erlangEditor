%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2016 21:55
%%%-------------------------------------------------------------------
-module(cursesDisplay).
-author("nietaki").

-behaviour(gen_server).
-include("../deps/cecho/include/cecho.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0]).

%% export needed to run the input loo
-export([getch_loop/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    text,
    cursorPosition
}).

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
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    cluster_utils:join_server_cluster(),
    Name = "Jacek",
    RegisterResult = ledgerServer:register(Name),
    case RegisterResult of
        {ledger_head_state, _HeadId, Text} ->
            % start cecho
            ok = cecho:cbreak(),
            ok = cecho:noecho(),
            ok = cecho:keypad(?ceSTDSCR, true),
            ok = cecho:curs_set(?ceCURS_NORMAL),
            State = starting_state(Text),
            repaint(State),
            % spawn input capturing loop
            spawn_link(cursesDisplay, getch_loop, [?MODULE]),
            %return
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
        State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({ch, ?ceKEY_ESC}, State) -> 
    erlangEditor:stop(),
    {stop, user_pressed_esc, State};
handle_cast({ch, Ch}, State) ->
    cecho:mvaddch(10, 10, Ch),
    NewState = handle_char(State, Ch),
    repaint(NewState),
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
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
        State :: #state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
        Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%starting_state() -> #state{text = "Initial text in the editor", cursorPosition = 5}.
starting_state(InitialText) -> #state{text = InitialText, cursorPosition = length(InitialText)}.

getch_loop(ServerRef) ->
    Ch = cecho:getch(),
    gen_server:cast(ServerRef, {ch, Ch}),
    getch_loop(ServerRef).

-spec(repaint(State :: #state{}) -> ok).
repaint(State) -> 
    #state{text = Text, cursorPosition = Pos} = State,
    ok = cecho:erase(),
    ok = cecho:mvaddstr(0, 0, Text),
    {_Height, Width} = cecho:getmaxyx(),
    ok = render_lines(0, split_into_lines(Width, Text)),
    {Y, X} = get_yx_from_position(cecho:getmaxyx(), Pos),
    ok = cecho:move(Y, X),
    ok = cecho:refresh(),
    ok.

render_lines(_StartingLineNumber, []) -> ok;
render_lines(StartingLineNumber, [H|T]) ->
    cecho:mvaddstr(StartingLineNumber, 0, H),
    render_lines(StartingLineNumber + 1, T).
    

-spec(split_into_lines(LineLength :: integer(), Text :: list()) -> [list()]).
split_into_lines(LineLength, Text) ->
    lists:reverse(split_into_lines_1(LineLength, Text, [])).

split_into_lines_1(_LineLength, [], Acc) -> Acc;
split_into_lines_1(LineLength, Text, Acc) ->
    {First, Rest} = stringOps:split(LineLength, Text),
    split_into_lines_1(LineLength, Rest, [First|Acc]).


split_into_lines_test_() -> [
    ?_assertEqual([], split_into_lines(10, "")),
    ?_assertEqual(["a", "b"], split_into_lines(1, "ab")),
    ?_assertEqual(["ab", "c"], split_into_lines(2, "abc"))
].

-spec(handle_char(State :: #state{}, Ch ::char()) -> #state{}).
handle_char(State, Ch) ->
    YX = cecho:getmaxyx(),
    CursorPosition = State#state.cursorPosition,
    NewState = case Ch of
        Printable when Printable >= 20, Printable =< $~ -> insert_character(State, Printable, CursorPosition);
        ?ceKEY_DEL -> delete_character(State, CursorPosition);
        ?ceKEY_DOWN -> State#state{cursorPosition = get_new_position(CursorPosition, YX, down)};
        ?ceKEY_UP -> State#state{cursorPosition = get_new_position(CursorPosition, YX, up)};
        ?ceKEY_LEFT -> State#state{cursorPosition = get_new_position(CursorPosition, YX, left)};
        ?ceKEY_RIGHT -> State#state{cursorPosition = get_new_position(CursorPosition, YX, right)};
        ?ceKEY_BACKSPACE -> delete_character(State, CursorPosition - 1);
        ?ceKEY_BACKSPACE2-> delete_character(State, CursorPosition - 1);
        _ -> State           
        %SomethingElse -> error(SomethingElse) 
    end,
    fixup_state_position(NewState).
  
fixup_state_position(#state{text = Text, cursorPosition = Position}) ->
    #state{text = Text, cursorPosition = max(min(Position, string:len(Text)), 0)}.

insert_character(#state{text = Text, cursorPosition = CurrentPosition}, Character, Position) -> 
    NewCursorPosition = if
        Position =< CurrentPosition-> CurrentPosition + 1;
        true -> CurrentPosition
    end,
    #state{text = stringOps:insert_char(Text, Character, Position), cursorPosition = NewCursorPosition}.

delete_character(#state{text = Text, cursorPosition = CurrentPosition}, PositionToDelete) ->
    NewCursorPosition = if
        PositionToDelete < CurrentPosition -> CurrentPosition - 1;
        true -> CurrentPosition
    end, 
    #state{text = stringOps:delete_char(Text, PositionToDelete), cursorPosition = NewCursorPosition}.

get_new_position(OriginalPosition, {ConsoleHeight, ConsoleWidth}, Direction) ->
    NewPosition = case Direction of
        down -> OriginalPosition + ConsoleWidth;
        up -> OriginalPosition - ConsoleWidth;
        left -> OriginalPosition - 1;
        right -> OriginalPosition + 1
        %_ -> OriginalPosition 
    end,
    min(max(0, NewPosition), ConsoleHeight * ConsoleWidth - 1).

get_yx_from_position({_ConsoleHeight, ConsoleWidth}, Position) ->
    {Position div ConsoleWidth, Position rem ConsoleWidth}.
    

