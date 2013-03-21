%%==============================================================================
%% Copyright 2013 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%
%% This file is part of jhn_stdlib_props.
%%
%% jhn_stdlib_props is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% jhn_stdlib_props is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with jhn_stdlib_props.  If not, see <http://www.gnu.org/licenses/>.
%%==============================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%    These tests are to ensure that bstring is a drop in replacement
%%    for the string module in stdlib in the scope of bstring.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(bstring_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

-define(EXCEMPT_SPECS,
        [{substr, 2}, {substr, 3},
         {sub_string, 2}, {sub_string, 3}
        ]).

-include("jhn_stdlib_proper.hrl").

-type direction() :: left | right | both.

-define(EQUAL_WHEN_CAUGHT(X, Y),
        case {catch {ok, X}, catch {ok, Y}} of
            {{ok, V}, {ok, V}} ->
                true;
            {{ok, _}, _} ->
                false;
            {_, {ok, _}} ->
                false;
            _ ->
                true
        end).

prop_len_1() ->
    ?FORALL(
       String,
       list(byte()),
       string:len(String) == bstring:len(list_to_binary(String))).

prop_equal_2_true() ->
    ?FORALL(
       String,
       list(byte()),
       bstring:equal(list_to_binary(String), list_to_binary(String))).

prop_equal_2_random() ->
    ?FORALL(
       {String1, String2},
       {list(byte()), list(byte())},
       string:equal(String1, String2) ==
           bstring:equal(list_to_binary(String1), list_to_binary(String2))).

prop_concat_2() ->
    ?FORALL(
       {String1, String2},
       {list(byte()), list(byte())},
       string:concat(String1, String2) ==
           binary_to_list(bstring:concat(list_to_binary(String1),
                                             list_to_binary(String2)))).

prop_chr_2() ->
    ?FORALL(
       {String, Char},
       {list(byte()), byte()},
       string:chr(String, Char) ==
           bstring:chr(list_to_binary(String), Char)).

prop_rchr_2() ->
    ?FORALL(
       {String, Char},
       {list(byte()), byte()},
       string:rchr(String, Char) ==
           bstring:rchr(list_to_binary(String), Char)).

prop_str_2_true() ->
    ?FORALL(
       {String1, String2b, String2e},
       {list(byte()), list(byte()), list(byte())},
       string:str(String2b ++ String1 ++ String2e, String1) ==
           bstring:str(list_to_binary(String2b ++ String1 ++ String2e),
                           list_to_binary(String1))).

prop_str_2_random() ->
    ?FORALL(
       {String1, String2},
       {list(byte()), list(byte())},
       string:str(String1, String2) ==
           bstring:str(list_to_binary(String1), list_to_binary(String2))).

prop_rstr_2_true() ->
    ?FORALL(
       {String1, String2b, String2e},
       {list(byte()), list(byte()), list(byte())},
       string:rstr(String2b ++ String1 ++ String2e, String1) ==
           bstring:rstr(list_to_binary(String2b ++ String1 ++ String2e),
                            list_to_binary(String1))).

prop_rstr_2_random() ->
    ?FORALL(
       {String1, String2},
       {list(byte()), list(byte())},
       string:rstr(String1, String2) ==
           bstring:rstr(list_to_binary(String1), list_to_binary(String2))).

prop_span_2_random() ->
    ?FORALL(
       {String1, String2},
       {list(byte()), list(byte())},
       string:span(String1, String2) ==
           bstring:span(list_to_binary(String1), list_to_binary(String2))).

prop_cspan_2_random() ->
    ?FORALL(
       {String1, String2},
       {list(byte()), list(byte())},
       string:cspan(String1, String2) ==
           bstring:cspan(list_to_binary(String1), list_to_binary(String2))).

prop_substr_2_random() ->
    ?FORALL(
       {String, Start},
       {list(byte()), non_neg_integer()},
       ?EQUAL_WHEN_CAUGHT(
          string:substr(String, Start),
          binary_to_list(bstring:substr(list_to_binary(String), Start)))).

prop_substr_3_random() ->
    ?FORALL(
       {String, Start, Length},
       {list(byte()), pos_integer(), non_neg_integer()},
       ?EQUAL_WHEN_CAUGHT(
          string:substr(String, Start, Length),
          binary_to_list(
            bstring:substr(list_to_binary(String), Start, Length)))).

prop_tokens_2_random() ->
    ?FORALL(
       {String, Separators},
       {list(byte()), list(byte())},
       string:tokens(String, Separators) ==
           [binary_to_list(E) ||
               E <- bstring:tokens(list_to_binary(String),
                                       list_to_binary(Separators))]).

prop_join_2_random() ->
    ?FORALL(
       {Strings, Separator},
       {list(list(byte())), list(byte())},
       string:join(Strings, Separator) ==
           binary_to_list(
             bstring:join([list_to_binary(String) || String <- Strings],
                              list_to_binary(Separator)))).

prop_chars_2() ->
    ?FORALL(
       {Char, No},
       {byte(), non_neg_integer()},
       string:chars(Char, No) ==
          binary_to_list(bstring:chars(Char, No))).

prop_chars_3() ->
    ?FORALL(
       {Char, No, Tail},
       {byte(), non_neg_integer(), list(byte())},
       string:chars(Char, No, Tail) ==
          binary_to_list(bstring:chars(Char, No, list_to_binary(Tail)))).

prop_copies_2() ->
    ?FORALL(
       {String, No},
       {list(byte()), non_neg_integer()},
       string:copies(String, No) ==
          binary_to_list(bstring:copies(list_to_binary(String), No))).

prop_words_1() ->
    ?FORALL(
       String,
       list(byte()),
       string:words(String) == bstring:words(list_to_binary(String))).

prop_words_2() ->
    ?FORALL(
       {String, Char},
       {list(byte()), byte()},
       string:words(String, Char) ==
           bstring:words(list_to_binary(String), Char)).

prop_words_2_separator() ->
    ?FORALL(
       {String, Char},
       {list(byte()), byte()},
       string:words([Char | String], Char) ==
           bstring:words(list_to_binary([Char | String]), Char)).

prop_sub_word_2() ->
    ?FORALL(
       {String, No},
       {list(byte()), pos_integer()},
       string:sub_word(String, No) ==
           binary_to_list(bstring:sub_word(list_to_binary(String), No))).

prop_sub_word_3() ->
    ?FORALL(
       {String, No, Char},
       {list(byte()), pos_integer(), byte()},
       string:sub_word(String, No, Char) ==
           binary_to_list(
             bstring:sub_word(list_to_binary(String), No, Char))).

prop_sub_word_3_separator() ->
    ?FORALL(
       {String, No, Char},
       {list(byte()), pos_integer(), byte()},
       string:sub_word([Char | String], No, Char) ==
           binary_to_list(
             bstring:sub_word(list_to_binary([Char | String]), No, Char))).

prop_strip_1() ->
    ?FORALL(
       String,
       list(byte()),
       string:strip(String) ==
           binary_to_list(bstring:strip(list_to_binary(String)))).

prop_strip_2() ->
    ?FORALL(
       {String, Direction},
       {list(byte()), direction()},
       string:strip(String, Direction) ==
           binary_to_list(
             bstring:strip(list_to_binary(String), Direction))).

prop_strip_3() ->
    ?FORALL(
       {String, Direction, Char},
       {list(byte()), direction(), char()},
       string:strip(String, Direction, Char) ==
           binary_to_list(
             bstring:strip(list_to_binary(String), Direction, Char))).

prop_left_2() ->
    ?FORALL(
       {String, Length},
       {list(byte()), non_neg_integer()},
       string:left(String, Length) ==
           binary_to_list(
             bstring:left(list_to_binary(String), Length))).

prop_left_3() ->
    ?FORALL(
       {String, Length, Char},
       {list(byte()), non_neg_integer(), byte()},
       string:left(String, Length, Char) ==
           binary_to_list(
             bstring:left(list_to_binary(String), Length, Char))).

prop_right_2() ->
    ?FORALL(
       {String, Length},
       {list(byte()), non_neg_integer()},
       string:right(String, Length) ==
           binary_to_list(
             bstring:right(list_to_binary(String), Length))).

prop_right_3() ->
    ?FORALL(
       {String, Length, Char},
       {list(byte()), non_neg_integer(), byte()},
       string:right(String, Length, Char) ==
           binary_to_list(
             bstring:right(list_to_binary(String), Length, Char))).

prop_centre_2() ->
    ?FORALL(
       {String, Length},
       {list(byte()), non_neg_integer()},
       string:centre(String, Length) ==
           binary_to_list(
             bstring:centre(list_to_binary(String), Length))).

prop_centre_3() ->
    ?FORALL(
       {String, Length, Char},
       {list(byte()), non_neg_integer(), byte()},
       string:centre(String, Length, Char) ==
           binary_to_list(
             bstring:centre(list_to_binary(String), Length, Char))).

prop_sub_string_2_random() ->
    ?FORALL(
       {String, Start},
       {list(byte()), non_neg_integer()},
       ?EQUAL_WHEN_CAUGHT(
          string:sub_string(String, Start),
          binary_to_list(
            bstring:sub_string(list_to_binary(String), Start)))).

prop_sub_string_3_random() ->
    ?FORALL(
       {String, Start, Length},
       {list(byte()), non_neg_integer(), non_neg_integer()},
       ?EQUAL_WHEN_CAUGHT(
          string:sub_string(String, Start, Length),
          binary_to_list(
            bstring:sub_string(list_to_binary(String), Start, Length)))).

prop_to_integer_1_integer() ->
    ?FORALL(
       Integer,
       integer(),
       to_number_equal(
         string:to_integer(integer_to_list(Integer)),
         bstring:to_integer(list_to_binary(integer_to_list(Integer))))).

prop_to_integer_1_random() ->
    ?FORALL(
       String,
       list(byte()),
       to_number_equal(
         string:to_integer(String),
         bstring:to_integer(list_to_binary(String)))).

prop_to_float_1_float() ->
    ?FORALL(
       Float,
       float(),
       to_number_equal(
         string:to_float(float_to_list(Float)),
         bstring:to_float(list_to_binary(float_to_list(Float))))).

prop_to_float_1_random() ->
    ?FORALL(
       String,
       list(byte()),
       to_number_equal(
         string:to_float(String),
         bstring:to_float(list_to_binary(String)))).

to_number_equal({error, _}, {error, _}) ->
    true;
to_number_equal({Integer, String}, {Integer, Binary}) ->
    String == binary_to_list(Binary);
to_number_equal(_, _) ->
    false.

prop_to_lower_1() ->
    ?FORALL(
       String,
       list(byte()),
       string:to_lower(String) ==
           binary_to_list(bstring:to_lower(list_to_binary(String)))).

prop_to_upper_1() ->
    ?FORALL(
       String,
       list(byte()),
       string:to_upper(String) ==
           binary_to_list(bstring:to_upper(list_to_binary(String)))).
