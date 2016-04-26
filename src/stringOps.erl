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
-export([insert_char/3, delete_char/2]).

insert_char(String, Character, Position) ->
    notok.

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