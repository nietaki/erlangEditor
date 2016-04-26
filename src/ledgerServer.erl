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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

% clients is a map of Pid/contact name, to last seen head_id
-record(state, {head_id = 0, head_text = "", clients = #{}, changes = []}).

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
    {ok, #state{}}.

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
handle_call(register, From, State) ->
    #state{head_id = HeadId, clients = Clients} = State,
    NewClients = Clients#{get_client_pid(From) => HeadId},
    {reply, get_ledger_state_message(State), State#state{clients = NewClients}};
handle_call(get_state, _From, State) ->
    {reply, State, State};
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
handle_cast({submit_local_changes, Pid, BaseHeadId, NewChanges}, State) ->
    #state{head_id = StateHeadId} = State,
    case BaseHeadId of
        StateHeadId -> 
            case lists:all(fun(X) -> is_a_change(X) end, NewChanges) of
                false -> {noreply, State}; %changes were broken
                true ->
                    case apply_changes(State#state.head_text, NewChanges) of
                        {ok, NewText} -> 
                            OldId = State#state.head_id,
                            NewId = OldId + length(NewChanges),
                            OldChanges = State#state.changes,
                            ClientsMap = State#state.clients,
                            NewClientsMap = ClientsMap#{Pid := NewId},
                            gen_server:cast(Pid, {local_changes_accepted, OldId, length(NewChanges)}),
                            {noreply, State#state{head_id = NewId, clients = NewClientsMap, head_text = NewText, changes = OldChanges ++ NewChanges}};
                        _ -> 
                            {noreply, State} %changes couldn't be applied
                    end
            end; 
        Older when Older < StateHeadId -> {noreply, State}; %TODO send the changes since older
        Newer when Newer > StateHeadId -> error(received_head_id_too_new)
    end;
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

get_ledger_state_message(#state{head_id = HeadId, head_text = Text}) ->
    {ledger_state, HeadId, Text}.

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
