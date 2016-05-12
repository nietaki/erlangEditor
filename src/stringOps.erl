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

%UP NEXT: rebase_changes(MasterChanges, LocalChanges) -> RebasedLocalChanges
