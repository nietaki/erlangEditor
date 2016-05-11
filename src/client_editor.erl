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
    Name = "Jacek",
    RegisterResult = ledgerServer:register(Name),
    case RegisterResult of
        {ledger_head_state, _HeadId, Text} ->
            {ok, DisplayYX} = DisplayInitFun(self()),
            State = #client_state{text = Text, cursorPosition = length(Text), display_repaint_fun = DisplayRepaintFun, display_yx = DisplayYX},
            Repaint = State#client_state.display_repaint_fun,
            ok = Repaint(State),
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
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
handle_cast({ch, Ch}, State) ->
    NewState = handle_char(State, Ch),
    RepaintFun = State#client_state.display_repaint_fun,
    RepaintFun = NewState#client_state.display_repaint_fun,
    RepaintFun(NewState),
    %repaint(NewState),
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

-spec(handle_char(State :: #client_state{}, Ch ::char()) -> #client_state{}).
handle_char(State, Ch) ->
    YX = State#client_state.display_yx,
    CursorPosition = State#client_state.cursorPosition,
    NewState = case Ch of
        Printable when Printable >= 20, Printable =< $~ -> insert_character(State, Printable, CursorPosition);
        ?ceKEY_DEL -> delete_character(State, CursorPosition);
        ?ceKEY_DOWN -> State#client_state{cursorPosition = get_new_position(CursorPosition, YX, down)};
        ?ceKEY_UP -> State#client_state{cursorPosition = get_new_position(CursorPosition, YX, up)};
        ?ceKEY_LEFT -> State#client_state{cursorPosition = get_new_position(CursorPosition, YX, left)};
        ?ceKEY_RIGHT -> State#client_state{cursorPosition = get_new_position(CursorPosition, YX, right)};
        ?ceKEY_BACKSPACE -> delete_character(State, CursorPosition - 1);
        ?ceKEY_BACKSPACE2-> delete_character(State, CursorPosition - 1);
        _ -> State           
        %SomethingElse -> error(SomethingElse) 
    end,
    fixup_state_position(NewState).
  
fixup_state_position(#client_state{text = Text, cursorPosition = Position}=State) ->
    State#client_state{text = Text, cursorPosition = max(min(Position, string:len(Text)), 0)}.

insert_character(#client_state{text = Text, cursorPosition = CurrentPosition}=State, Character, Position) -> 
    NewCursorPosition = if
        Position =< CurrentPosition-> CurrentPosition + 1;
        true -> CurrentPosition
    end,
    State#client_state{text = stringOps:insert_char(Text, Character, Position), cursorPosition = NewCursorPosition}.

delete_character(#client_state{text = Text, cursorPosition = CurrentPosition}=State, PositionToDelete) ->
    NewCursorPosition = if
        PositionToDelete < CurrentPosition -> CurrentPosition - 1;
        true -> CurrentPosition
    end, 
    State#client_state{text = stringOps:delete_char(Text, PositionToDelete), cursorPosition = NewCursorPosition}.

get_new_position(OriginalPosition, {ConsoleHeight, ConsoleWidth}, Direction) ->
    NewPosition = case Direction of
        down -> OriginalPosition + ConsoleWidth;
        up -> OriginalPosition - ConsoleWidth;
        left -> OriginalPosition - 1;
        right -> OriginalPosition + 1
        %_ -> OriginalPosition 
    end,
    min(max(0, NewPosition), ConsoleHeight * ConsoleWidth - 1).
    

