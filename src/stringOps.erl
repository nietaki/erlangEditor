%%%-------------------------------------------------------------------
%%% @author nietaki
%%% @copyright (C) 2016, nietaki.github.io
%%% @doc
%%%
%%% @end
%%% Created : 26. Apr 2016 16:10
%%%-------------------------------------------------------------------
-module(stringOps).
-author("nietaki").

-include_lib("eunit/include/eunit.hrl").
%% API
-export([insert_char/3, delete_char/2, split/2, is_a_change/1, apply_change/2, apply_changes/2]).
-export([apply_change_to_position/2, apply_changes_to_position/2]).

delete_char(String, Position) ->
    lists:reverse(delete_character_1(String, Position, [])).

delete_character_1([], _, Acc) -> Acc;
delete_character_1([_|T], 0, Acc) -> delete_character_1(T, -1, Acc);
delete_character_1([H|T], X, Acc) -> delete_character_1(T, X-1, [H|Acc]).

delete_character_test_() ->
    [
        ?_assertEqual("", delete_char("", 0)),
        ?_assertEqual("", delete_char("", 5)),
        ?_assertEqual("", delete_char("a", 0)),
        ?_assertEqual("a", delete_char("a", 1)),
        ?_assertEqual("bc", delete_char("abc", 0)),
        ?_assertEqual("ac", delete_char("abc", 1)),
        ?_assertEqual("ab", delete_char("abc", 2))
    ].

insert_char(String, Character, Position) ->
    lists:reverse(insert_char_1(String, Character, Position, "")).

insert_char_1([], Ch, 0, Acc) -> [Ch|Acc];
insert_char_1([], _, _, Acc) -> Acc;
insert_char_1([H|T], Ch, 0, Acc) -> insert_char_1(T, Ch, -1, [H|[Ch|Acc]]);
insert_char_1([H|T], Ch, X, Acc) -> insert_char_1(T, Ch, X-1, [H|Acc]).

insert_char_test_() ->
    [
        ?_assertEqual("x", insert_char("", $x, 0)),
        ?_assertEqual("", insert_char("", $x, 1)),
        ?_assertEqual("", insert_char("", $x, -1)),
        ?_assertEqual("xb", insert_char("b", $x, 0)),
        ?_assertEqual("ax", insert_char("a", $x, 1)),
        ?_assertEqual("a", insert_char("a", $x, 2)),
        ?_assertEqual("axc", insert_char("ac", $x, 1))
    ].

% robust version of lists:split
-spec(split(N :: integer(), List1:: [any()]) -> {[any()], [any()]}).
split(N, _List1) when N < 1 -> error("cannot split into bits smaller than 1");
split(N, List1) ->
    {First, Second} = split_1(N, [], List1),
    {lists:reverse(First), Second}.

split_1(0, Acc, Remaining) -> {Acc, Remaining};
split_1(_N, Acc, []) -> {Acc, []};
split_1(N, Acc, [H|T]) -> split_1(N - 1, [H|Acc], T).

split_test_() -> [
    ?_assertEqual({"", ""}, split(1, "")),
    ?_assertEqual({"", ""}, split(5, "")),
    ?_assertEqual({"ab", "cde"}, split(2, "abcde")),
    ?_assertEqual({"abcde", ""}, split(5, "abcde")),
    ?_assertEqual({"abcde", ""}, split(665, "abcde"))
].

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

% moves the position of something in the text to where it will be after the change is applied to the text
apply_change_to_position({insert_char, ChangePos, _Char}, Position) when ChangePos =< Position -> Position + 1;
apply_change_to_position({insert_char, _ChangePos, _Char}, Position) -> Position;
apply_change_to_position({delete_char, ChangePos}, Position) when ChangePos =< Position -> max(0, Position - 1);
apply_change_to_position({delete_char, _ChangePos}, Position) -> Position.
   
apply_change_to_position_test_() -> [
    ?_assertEqual(4, apply_change_to_position({insert_char, 0, $x}, 3)),
    ?_assertEqual(1, apply_change_to_position({insert_char, 0, $x}, 0)),
    ?_assertEqual(0, apply_change_to_position({insert_char, 5, $x}, 0)),
    ?_assertEqual(4, apply_change_to_position({insert_char, 100, $x}, 4)),
    % deletes
    ?_assertEqual(4, apply_change_to_position({delete_char, 1}, 5)),
    ?_assertEqual(4, apply_change_to_position({delete_char, 0}, 5)),
    ?_assertEqual(0, apply_change_to_position({delete_char, 0}, 0)),
    ?_assertEqual(5, apply_change_to_position({delete_char, 9}, 5)),
    ?_assertEqual(0, apply_change_to_position({delete_char, 9}, 0))
].

apply_changes_to_position([], Position) -> Position;
apply_changes_to_position([H|T], Position) -> apply_changes_to_position(T, apply_change_to_position(H, Position)).

apply_changes_to_position_test_() -> [
    ?_assertEqual(4, apply_changes_to_position([{insert_char, 0, $x}], 3)),
    ?_assertEqual(4, apply_changes_to_position([{insert_char, 0, $x}, {insert_char, 9, $x}], 3)),
    ?_assertEqual(5, apply_changes_to_position([{insert_char, 0, $x}, {insert_char, 1, $x}], 3)),
    ?_assertEqual(5, apply_changes_to_position([{insert_char, 0, $x}, {insert_char, 0, $x}], 3)),
    ?_assertEqual(3, apply_changes_to_position([{insert_char, 0, $x}, {delete_char, 0}], 3)),
    ?_assertEqual(6, apply_changes_to_position([{insert_char, 0, $x}, {insert_char, 0, $x}, {insert_char, 4, $x}], 3))
].

%MAIN ATTRACTION: rebase_changes(MasterChanges, LocalChanges) -> RebasedLocalChanges
rebase_changes([] = _RemoteChanges, LocalChanges) -> LocalChanges;
rebase_changes([H|T], LocalChanges) -> rebase_changes(T, rebase_change(H, LocalChanges)).

%rebase_change(RemoteChange, LocalChanges) -> [NewChange].
rebase_change(MasterChange, LocalChanges) ->
    ReversedResult = rebase_change_inner(MasterChange, LocalChanges, []),
    lists:reverse(ReversedResult).

% MasterChange will be "projected" as the execution goes through the remaining changes
%rebase_change_inner(MasterChange, RemainingLocalChanges, Acc) ->
% base case
rebase_change_inner(_ProjectedMasterChange, [], Acc) -> Acc;
% both inserts
rebase_change_inner({insert_char, X, _} = MasterChange, [{insert_char, Y, YChar}|T], Acc) when X =< Y ->
    rebase_change_inner(MasterChange, T, [{insert_char, Y + 1, YChar} | Acc]);
rebase_change_inner({insert_char, X, C} = _MasterChange, [{insert_char, Y, YChar}|T], Acc) when X > Y ->
    rebase_change_inner({insert_char, X + 1, C}, T, [{insert_char, Y, YChar} | Acc]);
% right delete
rebase_change_inner({insert_char, X, _} = MasterChange, [{delete_char, Y}|T], Acc) when X =< Y ->
    rebase_change_inner(MasterChange, T, [{delete_char, Y + 1} | Acc]);
rebase_change_inner({insert_char, X, C} = _MasterChange, [{delete_char, Y}|T], Acc) when X > Y ->
    rebase_change_inner({insert_char, X - 1, C}, T, [{delete_char, Y} | Acc]);
% left delete
rebase_change_inner({delete_char, X} = MasterChange, [{insert_char, Y, C}|T], Acc) when X < Y ->
    rebase_change_inner(MasterChange, T, [{insert_char, Y - 1, C} | Acc]);
rebase_change_inner({delete_char, X} = _MasterChange, [{insert_char, Y, C}|T], Acc) when X >= Y ->
    rebase_change_inner({delete_char, X + 1}, T, [{insert_char, Y, C} | Acc]).

rebase_testcase(InitialText, MasterChanges, LocalChanges, ExpectedResultingText) ->
    RebasedChanges = rebase_changes(MasterChanges, LocalChanges),
    {ok, AfterMaster} = apply_changes(InitialText, MasterChanges),
    {ok, ResultingText} = apply_changes(AfterMaster, RebasedChanges),
    ?assertEqual(ExpectedResultingText, ResultingText).

rebase_changes_test_() -> [
    fun () -> rebase_testcase("foo", [], [], "foo") end,
    fun () -> rebase_testcase("foo", [], [{insert_char, 3, $l}], "fool") end,
    fun () -> rebase_testcase("foo", [{insert_char, 2, $d}], [{insert_char, 3, $l}], "fodol") end,
    fun () -> rebase_testcase("foo", [{insert_char, 3, $d}], [{insert_char, 2, $l}], "folod") end,
    fun () -> rebase_testcase("foo", [{insert_char, 3, $d}], [{insert_char, 3, $y}, {insert_char, 2, $x}], "foxody") end,
    % multiple changes in master
    fun () -> rebase_testcase("abc", [{insert_char, 3, $Y},{insert_char, 0, $X}], [{insert_char, 1, $x}], "XaxbcY") end,
    % deletes on the right side
    fun () -> rebase_testcase("abc", [{insert_char, 0, $X}], [{delete_char, 2}], "Xab") end,
    fun () -> rebase_testcase("abc", [{insert_char, 1, $X}], [{delete_char, 1}], "aXc") end,
    fun () -> rebase_testcase("abc", [{insert_char, 3, $X}], [{delete_char, 1}], "acX") end,
    fun () -> rebase_testcase("abc", [{insert_char, 2, $X}], [{delete_char, 1}, {insert_char, 2, $y}], "aXcy") end,
    fun () -> rebase_testcase("abc", [{insert_char, 2, $X}], [{delete_char, 1}, {insert_char, 1, $y}], "aXyc") end,
    % deletes on the left side
    fun () -> rebase_testcase("abc", [{delete_char, 1}], [{insert_char, 2, $X}], "aXc") end,
    fun () -> rebase_testcase("abc", [{delete_char, 1}], [{insert_char, 3, $X}], "acX") end,
    fun () -> rebase_testcase("abc", [{delete_char, 1}], [{insert_char, 1, $X}], "aXc") end,
    fun () -> rebase_testcase("abc", [{delete_char, 1}], [{insert_char, 0, $X}], "Xac") end,
    fun () -> rebase_testcase("removeme", [], [], "removeme") end
].
