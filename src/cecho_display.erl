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
-export([initialize/1, repaint/2]).

-export([getch_loop/1]).

initialize(ServerRef) -> 
    % start cecho
    ok = cecho:cbreak(),
    ok = cecho:noecho(),
    % colors
    ok = cecho:keypad(?ceSTDSCR, true),
    ok = cecho:curs_set(?ceCURS_NORMAL),
    YX = cecho:getmaxyx(),
    spawn_link(cecho_display, getch_loop, [ServerRef]),
    {ok, YX}.

getch_loop(ServerRef) ->
    Ch = cecho:getch(),
    client_editor:send_char(ServerRef, Ch),
    getch_loop(ServerRef).

%this will accept client_state initially, display_state later on
-spec(repaint(State :: #local_state{}, CursorPositions :: [{string(), integer()}]) -> {ok, integer()}).
repaint(#local_state{resulting_text = Text, cursor_position = Pos}, CursorPositions) -> 
    ok = cecho:erase(),
    ok = cecho:mvaddstr(0, 0, Text),
    {_Height, Width} = cecho:getmaxyx(),
    ok = render_lines(0, split_into_lines(Width, Text)),
    print_cursors(CursorPositions),
    {Y, X} = get_yx_from_position(cecho:getmaxyx(), Pos),
    ok = cecho:move(Y, X),
    ok = cecho:refresh(),
    ok.

-spec(print_cursors(Cursors :: [{string(), integer()}]) -> ok).
print_cursors(Cursors) ->
    YX = cecho:getmaxyx(),
    cecho:attron(?ceA_BOLD),
    lists:map(fun({Name, Position}) ->  
        {Y, X} = get_yx_from_position(YX, Position),
        Char = hd(Name),
        cecho:mvaddch(Y, X, Char)
             end, Cursors),
    cecho:attroff(?ceA_BOLD).

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
