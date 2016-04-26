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
    % start cecho
    ok = cecho:cbreak(),
    ok = cecho:noecho(),
    ok = cecho:keypad(?ceSTDSCR, true),
    ok = cecho:curs_set(?ceCURS_NORMAL),
    State = starting_state(),
    repaint(State),
    % spawn input capturing loop
    spawn_link(cursesDisplay, getch_loop, [?MODULE]),
    %return
    {ok, State}.

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

starting_state() -> #state{text = "Initial text in the editor", cursorPosition = 5}.

getch_loop(ServerRef) ->
    Ch = cecho:getch(),
    gen_server:cast(ServerRef, {ch, Ch}),
    getch_loop(ServerRef).

-spec(repaint(State :: #state{}) -> ok).
repaint(State) -> 
    #state{text = Text, cursorPosition = Pos} = State,
    ok = cecho:erase(),
    ok = cecho:mvaddstr(0, 0, Text),
    {Y, X} = get_yx_from_position(cecho:getmaxyx(), Pos),
    ok = cecho:move(Y, X),
    ok = cecho:refresh(),
    ok.

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
        _ -> State
    end,
    fixup_state_position(NewState).
  
fixup_state_position(#state{text = Text, cursorPosition = Position}) ->
    #state{text = Text, cursorPosition = min(Position, string:len(Text))}.

map_key_to_direction(DirectionChar) ->
    case DirectionChar of
        ?ceKEY_DOWN -> down;
        ?ceKEY_UP -> up;
        ?ceKEY_LEFT -> left;
        ?ceKEY_RIGHT -> right;
        _ -> none
    end.

insert_character(State, Character, Position) -> State.
    
insert_character_into_string(String, Character, Position) ->
    notok.

delete_character(#state{text = Text, cursorPosition = CurrentPosition}, PositionToDelete) ->
    #state{text = delete_character_from_string(Text, PositionToDelete), cursorPosition = PositionToDelete}.

delete_character_from_string(String, Position) ->
    lists:reverse(delete_character_1(String, Position, [])).

delete_character_1([], _, Acc) -> Acc;
delete_character_1([H|T], 0, Acc) -> delete_character_1(T, -1, Acc);
delete_character_1([H|T], X, Acc) -> delete_character_1(T, X-1, [H|Acc]).

get_new_position(OriginalPosition, {ConsoleHeight, ConsoleWidth}, Direction) ->
    NewPosition = case Direction of
        down -> OriginalPosition + ConsoleWidth;
        up -> OriginalPosition - ConsoleWidth;
        left -> OriginalPosition - 1;
        right -> OriginalPosition + 1
        %_ -> OriginalPosition 
    end,
    min(max(0, NewPosition), ConsoleHeight * ConsoleWidth - 1).

get_yx_from_position({ConsoleHeight, ConsoleWidth}, Position) ->
    {Position div ConsoleWidth, Position rem ConsoleWidth}.
    

