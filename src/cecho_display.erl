%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2016 16:36
%%%-------------------------------------------------------------------
-module(cecho_display).
-author("nietaki").

-include("../deps/cecho/include/cecho.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("client_editor.hrl").

%% API
-export([initialize/1, repaint/1]).

-export([getch_loop/1]).

initialize(ServerRef) -> 
    % start cecho
    ok = cecho:cbreak(),
    ok = cecho:noecho(),
    ok = cecho:keypad(?ceSTDSCR, true),
    ok = cecho:curs_set(?ceCURS_NORMAL),
    YX = cecho:getmaxyx(),
    spawn_link(cecho_display, getch_loop, [ServerRef]),
    {ok, YX}.

getch_loop(ServerRef) ->
    Ch = cecho:getch(),
    gen_server:cast(ServerRef, {ch, Ch}),
    getch_loop(ServerRef).

%this will accept client_state initially, display_state later on
-spec(repaint(State :: #client_state{}) -> {ok, integer()}).
repaint(State) -> 
    #client_state{text = Text, cursorPosition = Pos} = State,
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

get_yx_from_position({_ConsoleHeight, ConsoleWidth}, Position) ->
    {Position div ConsoleWidth, Position rem ConsoleWidth}.
