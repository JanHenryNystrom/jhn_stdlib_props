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
%%    These tests are to ensure that blist is a drop in replacement
%%    for the lists module in stdlib in the scope of blist.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(blist_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').


-include("jhn_stdlib_proper.hrl").

-type thing() :: atom() | integer() | float() | list(byte()).

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

prop_all_2() ->
    ?FORALL(
       {Pred, String},
       {function([byte()], boolean()), list(byte())},
       lists:all(Pred, String) == blist:all(Pred, list_to_binary(String))).

prop_any_2() ->
    ?FORALL(
       {Pred, String},
       {function([byte()], boolean()), list(byte())},
       lists:any(Pred, String) == blist:any(Pred, list_to_binary(String))).

prop_append_1() ->
    ?FORALL(
       Strings,
       list(list(byte())),
       lists:append(Strings) ==
           binary_to_list(
             blist:append([list_to_binary(B) || B <- Strings]))).

prop_append_2() ->
    ?FORALL(
       {String1, String2},
       {list(byte()), list(byte())},
       lists:append(String1, String2) ==
           binary_to_list(
             blist:append(list_to_binary(String1),
                               list_to_binary(String2)))).

prop_concat_1() ->
    ?FORALL(
       Things,
       list(thing()),
       lists:concat(Things) ==
           binary_to_list(
             blist:concat(Things))).

prop_delete_2() ->
    ?FORALL(
       {Elt, String},
       {byte(), list(byte())},
       lists:delete(Elt, String) ==
           binary_to_list(blist:delete(Elt, list_to_binary(String)))).

prop_dropwhile_2() ->
    ?FORALL(
       {Pred, String},
       {function([byte()], boolean()), list(byte())},
       lists:dropwhile(Pred, String) ==
           binary_to_list(blist:dropwhile(Pred, list_to_binary(String)))).

prop_duplicate_2() ->
    ?FORALL(
       {N, Char},
       {pos_integer(), byte()},
       lists:duplicate(N, Char) ==
           binary_to_list(blist:duplicate(N, Char))).

prop_filter_2() ->
    ?FORALL(
       {Pred, String},
       {function([byte()], boolean()), list(byte())},
       lists:filter(Pred, String) ==
           binary_to_list(blist:filter(Pred, list_to_binary(String)))).

prop_flatlength_2() ->
    ?FORALL(
       IOList,
       iolist(),
       lists:flatlength(iolist_to_list(IOList)) ==
           blist:flatlength(IOList)).

iolist_to_list([]) -> [];
iolist_to_list(Binary) when is_binary(Binary) -> binary_to_list(Binary);
iolist_to_list([H | T]) when is_binary(H) ->
    [binary_to_list(H) | iolist_to_list(T)];
iolist_to_list([H | T]) when is_list(H) ->
    [iolist_to_list(H) | iolist_to_list(T)];
iolist_to_list([H | T]) when is_integer(H) ->
    [H | iolist_to_list(T)].

prop_flatmap_2() ->
    ?FORALL(
       {Fun, String},
       {function([byte()], list(byte())), list(byte())},
       lists:flatmap(Fun, String) ==
           binary_to_list(
             blist:flatmap(
               fun(C) -> list_to_binary(Fun(C)) end,
               list_to_binary(String)))).

prop_flatten_1() ->
    ?FORALL(
       IOList,
       iolist(),
       lists:flatten(iolist_to_list(IOList)) ==
           binary_to_list(blist:flatten(IOList))).

prop_flatten_2() ->
    ?FORALL(
       {IOList, Binary},
       {iolist(), binary()},
       lists:flatten(iolist_to_list(IOList), binary_to_list(Binary)) ==
           binary_to_list(blist:flatten(IOList, Binary))).


prop_foldl_3() ->
    ?FORALL(
       {Fun, Acc, String},
       {function([byte(), term()], term()), term(), list(byte())},
       lists:foldl(Fun, Acc, String) ==
               blist:foldl(Fun, Acc, list_to_binary(String))).

prop_foldr_3() ->
    ?FORALL(
       {Fun, Acc, String},
       {function([byte(), term()], term()), term(), list(byte())},
       lists:foldr(Fun, Acc, String) ==
               blist:foldr(Fun, Acc, list_to_binary(String))).

prop_foreach_2() ->
    ?FORALL(
       {Fun, String},
       {function([byte()], term()), list(byte())},
       lists:foreach(Fun, String) ==
           blist:foreach(Fun, list_to_binary(String))).

prop_keydelete_4() ->
     ?FORALL(
        {Key, {N, Size, Binary}},
        {byte(), key_list()},
        lists:keydelete(Key, N, to_key_list(Binary, Size)) ==
            to_key_list(blist:keydelete(Key, N, Size, Binary), Size)).

prop_keyfind_4() ->
     ?FORALL(
        {Key, {N, Size, Binary}},
        {byte(), key_list()},
        keyfind_comp(lists:keyfind(Key, N, to_key_list(Binary, Size)),
                     blist:keyfind(Key, N, Size, Binary),
                     Size)).

keyfind_comp(false, false, _) -> true;
keyfind_comp(TupleList, Binary, Size) ->
    TupleList == hd(to_key_list(Binary, Size)).

prop_keymap_4() ->
     ?FORALL(
        {Fun, {N, Size, Binary}},
        {function([byte()], byte()), key_list()},
        from_key_list(lists:keymap(Fun, N, to_key_list(Binary, Size))) ==
            blist:keymap(Fun, N, Size, Binary)).

prop_keymember_4() ->
     ?FORALL(
        {Key, {N, Size, Binary}},
        {byte(), key_list()},
        lists:keymember(Key, N, to_key_list(Binary, Size)) ==
            blist:keymember(Key, N, Size, Binary)).

key_list() ->
    ?LET({N, Size, Parts},
         size_and_pos(),
         {N, Size, binary(Size * Parts)}).

key_list_item() ->
    ?LET({N, Size, Parts},
         size_and_pos(),
         {N, Size, binary(Size * Parts), binary(Size)}).

key_lists() ->
    ?LET({N, Size, Parts1, Parts2},
         size_and_poses(),
         {N, Size, binary(Size * Parts1), binary(Size * Parts2)}).

size_and_pos() ->
    ?SUCHTHAT({N, Size, _Parts},
              {pos_integer(), pos_integer(), pos_integer()},
              N =< Size).

size_and_poses() ->
    ?SUCHTHAT({N, Size, _Parts1, _Parts2},
              {pos_integer(), pos_integer(), pos_integer(), pos_integer()},
              N =< Size).

from_key_list(TupleList) ->
    from_key_list(TupleList, <<>>).

from_key_list([], Acc) -> Acc;
from_key_list([Tuple | T], Acc) when is_tuple(Tuple) ->
    from_key_list(T, from_key_list(tuple_to_list(Tuple), Acc));
from_key_list([Int | T], Acc) when is_integer(Int) ->
    from_key_list(T, <<Acc/binary, Int>>).

to_key_list(Binary, Size) ->
    to_key_list(Binary, Size, []).

to_key_list(<<>>, _, Acc) -> lists:reverse(Acc);
to_key_list(Binary, Size, Acc) when byte_size(Binary)  == Size ->
    lists:reverse([list_to_tuple(binary_to_list(Binary)) | Acc]);
to_key_list(Binary, Size, Acc) ->
    to_key_list(binary_part(Binary, {Size, byte_size(Binary) - Size}),
                Size,
                [list_to_tuple(binary_to_list(binary_part(Binary, {0, Size}))) |
                 Acc]).

prop_keymerge_4() ->
     ?FORALL(
        {N, Size, Binary1, Binary2},
        key_lists(),
        from_key_list(
          lists:keymerge(N,
                         to_key_list(Binary1, Size),
                         to_key_list(Binary2, Size)))
        ==
            blist:keymerge(N, Size, Binary1, Binary2)).

prop_keyreplace_4() ->
     ?FORALL(
        {Key, {N, Size, Binary, Item}},
        {byte(), key_list_item()},
        from_key_list(
          lists:keyreplace(Key, N, to_key_list(Binary, Size),
                           hd(to_key_list(Item, Size)))) ==
            blist:keyreplace(Key, N, Size, Binary, Item)).

prop_keysearch_4() ->
     ?FORALL(
        {Key, {N, Size, Binary}},
        {byte(), key_list()},
        keysearch_comp(lists:keysearch(Key, N, to_key_list(Binary, Size)),
                       blist:keysearch(Key, N, Size, Binary),
                       Size)).


prop_keysort_3() ->
     ?FORALL(
        {N, Size, Binary},
        key_list(),
        from_key_list(
          lists:keysort(N, to_key_list(Binary, Size)))
        ==
            blist:keysort(N, Size, Binary)).


keysearch_comp(false, false, _) -> true;
keysearch_comp({value, TupleList}, {value, Binary}, Size) ->
    TupleList == hd(to_key_list(Binary, Size)).

prop_keystore_5() ->
     ?FORALL(
        {Key, {N, Size, Binary, Item}},
        {byte(), key_list_item()},
        from_key_list(
          lists:keystore(Key, N, to_key_list(Binary, Size),
                           hd(to_key_list(Item, Size)))) ==
            blist:keystore(Key, N, Size, Binary, Item)).

prop_keytake_4() ->
     ?FORALL(
        {Key, {N, Size, Binary}},
        {byte(), key_list()},
        keytake_comp(lists:keytake(Key, N, to_key_list(Binary, Size)),
                       blist:keytake(Key, N, Size, Binary),
                       Size)).


keytake_comp(false, false, _) -> true;
keytake_comp({value, Tuple, TupleList}, {value, Item, Binary}, Size) ->
    (TupleList == to_key_list(Binary, Size)) and
        (Tuple == hd(to_key_list(Item, Size))).


prop_last_1() ->
    ?FORALL(
       String,
       non_empty(list(byte())),
       lists:last(String) == blist:last(list_to_binary(String))).

prop_map_2() ->
    ?FORALL(
       {Fun, String},
       {function([byte()], byte()), list(byte())},
       lists:map(Fun, String) ==
           binary_to_list(blist:map(Fun, list_to_binary(String)))).

prop_mapfoldl_3() ->
    ?FORALL(
       {Fun, Acc, String},
       {function([byte(), term()], {byte(), term()}), term(), list(byte())},
       mapfoldl_comp(lists:mapfoldl(Fun, Acc, String),
                     blist:mapfoldl(Fun, Acc, list_to_binary(String)))).

prop_mapfoldr_3() ->
    ?FORALL(
       {Fun, Acc, String},
       {function([byte(), term()], {byte(), term()}), term(), list(byte())},
       mapfoldl_comp(lists:mapfoldr(Fun, Acc, String),
                     blist:mapfoldr(Fun, Acc, list_to_binary(String)))).

mapfoldl_comp({List, Term}, {Binary, Term}) ->
    List == binary_to_list(Binary);
mapfoldl_comp(_, _) ->
    false.

prop_max_1() ->
    ?FORALL(
       String,
       non_empty(list(byte())),
       lists:max(String) == blist:max(list_to_binary(String))).

prop_merge_1() ->
    ?FORALL(
       LList,
       list(list(byte())),
       lists:merge([lists:sort(L) || L <- LList]) ==
        binary_to_list(
          blist:merge([list_to_binary(lists:sort(L)) || L <- LList]))).

prop_merge_2() ->
    ?FORALL(
       {List1, List2},
       {list(byte()), list(byte())},
       lists:merge(lists:sort(List1), lists:sort(List2)) ==
        binary_to_list(
          blist:merge(list_to_binary(lists:sort(List1)),
                           list_to_binary(lists:sort(List2))))).

prop_merge_3() ->
    ?FORALL(
       {Pred, List1, List2},
       {function([byte(), byte()], boolean()), list(byte()), list(byte())},
       lists:merge(Pred, lists:sort(List1), lists:sort(List2)) ==
        binary_to_list(
          blist:merge(Pred,
                           list_to_binary(lists:sort(List1)),
                           list_to_binary(lists:sort(List2))))).

prop_merge3_3() ->
    ?FORALL(
       {List1, List2, List3},
       {list(byte()), list(byte()), list(byte())},
       lists:merge3(lists:sort(List1),
                    lists:sort(List2),
                    lists:sort(List3)) ==
        binary_to_list(
          blist:merge3(list_to_binary(lists:sort(List1)),
                            list_to_binary(lists:sort(List2)),
                            list_to_binary(lists:sort(List3))))).

prop_min_1() ->
    ?FORALL(
       String,
       non_empty(list(byte())),
       lists:min(String) == blist:min(list_to_binary(String))).

prop_nth_2() ->
    ?FORALL(
       {Pos, String},
       {pos_integer(), non_empty(list(byte()))},
       ?EQUAL_WHEN_CAUGHT(
          lists:nth(Pos, String),
          blist:nth(Pos, list_to_binary(String)))).

prop_nthtail_2() ->
    ?FORALL(
       {Pos, String},
       {pos_integer(), non_empty(list(byte()))},
       ?EQUAL_WHEN_CAUGHT(
          lists:nthtail(Pos, String),
          binary_to_list(blist:nthtail(Pos, list_to_binary(String))))).

prop_partition_2() ->
    ?FORALL(
       {Pred, List},
       {function([byte()], boolean()), list(byte())},
       lists:partition(Pred, List) ==
           list_to_tuple(
             [binary_to_list(X) ||
                 X <- tuple_to_list(
                        blist:partition(Pred, list_to_binary(List)))])).

prop_prefix_2() ->
    ?FORALL(
       {List1, List2},
       {list(byte()), list(byte())},
       lists:prefix(List1, List2) ==
           blist:prefix(list_to_binary(List1), list_to_binary(List2))).

prop_prefix_true_2() ->
    ?FORALL(
       {List1, List2},
       {list(byte()), list(byte())},
       lists:prefix(List1, List1 ++ List2) ==
           blist:prefix(list_to_binary(List1),
                             list_to_binary(List1 ++ List2))).

prop_seq_2() ->
    ?FORALL(
       {From, To},
       {byte(), byte()},
       ?EQUAL_WHEN_CAUGHT(
          lists:seq(From, To),
          binary_to_list(blist:seq(From, To)))).

prop_seq_3() ->
    ?FORALL(
       {From, To, Incr},
       {byte(), byte(), integer()},
       ?EQUAL_WHEN_CAUGHT(
          lists:seq(From, To, Incr),
          binary_to_list(blist:seq(From, To, Incr)))).

prop_seq_3_same() ->
    ?FORALL(
       {From, Incr},
       { byte(), integer()},
       ?EQUAL_WHEN_CAUGHT(
          lists:seq(From, From, Incr),
          binary_to_list(blist:seq(From, From, Incr)))).

prop_member_2() ->
    ?FORALL(
       String,
       list(byte()),
       lists:member($a, String) ==
           blist:member($a, list_to_binary(String))).

prop_reverse_1() ->
    ?FORALL(
       String,
       list(byte()),
       lists:reverse(String) ==
           binary_to_list(blist:reverse(list_to_binary(String)))).

prop_reverse_2() ->
    ?FORALL(
       {String, Tail},
       {list(byte()), list(byte())},
       lists:reverse(String, Tail) ==
           binary_to_list(blist:reverse(list_to_binary(String),
                                             list_to_binary(Tail)))).


prop_sort_1() ->
    ?FORALL(
       List,
       list(byte()),
       lists:sort(List) ==
        binary_to_list(
          blist:sort(list_to_binary(List)))).

prop_sort_2_leq() ->
    ?FORALL(
       List,
       list(byte()),
       lists:sort(fun leq/2, List) ==
           binary_to_list(
             blist:sort(fun leq/2, list_to_binary(List)))).

prop_sort_2_geq() ->
    ?FORALL(
       List,
       list(byte()),
       lists:sort(fun geq/2, List) ==
           binary_to_list(
             blist:sort(fun geq/2, list_to_binary(List)))).

leq(A, B) -> A =< B.

geq(A, B) -> A >= B.


prop_split_2() ->
    ?FORALL(
       {Pos, String},
       {non_neg_integer(), list(byte())},
       ?EQUAL_WHEN_CAUGHT(
          lists:split(Pos, String),
          begin
              {A, B} = blist:split(Pos, list_to_binary(String)),
              {binary_to_list(A), binary_to_list(B)}
          end)).


prop_splitwith_2() ->
    ?FORALL(
       {Pred, String},
       {function([byte()], boolean()), list(byte())},
       lists:splitwith(Pred, String) ==
           list_to_tuple([binary_to_list(E) ||
                             E <- tuple_to_list(
                                    blist:splitwith(
                                      Pred,
                                      list_to_binary(String)))])).

prop_sublist_2() ->
    ?FORALL(
       {String, Len},
       {list(byte()), pos_integer()},
       lists:sublist(String, Len) ==
              binary_to_list(blist:sublist(list_to_binary(String), Len))).


prop_sublist_3() ->
    ?FORALL(
       {String, Start, Len},
       {list(byte()), pos_integer(), pos_integer()},
       ?EQUAL_WHEN_CAUGHT(
          lists:sublist(String, Start, Len),
          binary_to_list(
            blist:sublist(list_to_binary(String), Start, Len)))).


prop_subtract_2() ->
    ?FORALL(
       {String1, String2},
       {list(byte()), list(byte())},
       lists:subtract(String1, String2) ==
              binary_to_list(blist:subtract(list_to_binary(String1),
                                                 list_to_binary(String2)))).


prop_suffix_2() ->
    ?FORALL(
       {String1, String2},
       {list(byte()), list(byte())},
       lists:suffix(String1, String2) ==
           blist:suffix(list_to_binary(String1), list_to_binary(String2))).

prop_sum_1() ->
    ?FORALL(
       String,
       list(byte()),
       lists:sum(String) ==
           blist:sum(list_to_binary(String))).

prop_takewhile_2() ->
    ?FORALL(
       {Pred, String},
       {function([byte()], boolean()), list(byte())},
       lists:takewhile(Pred, String) ==
           binary_to_list(blist:takewhile(Pred, list_to_binary(String)))).

prop_ukeymerge_4() ->
     ?FORALL(
        {N, Size, Binary1, Binary2},
        key_lists(),
        from_key_list(
          lists:ukeymerge(N,
                          lists:ukeysort(N, to_key_list(Binary1, Size)),
                          lists:ukeysort(N, to_key_list(Binary2, Size))))
        ==
            blist:ukeymerge(N,
                                 Size,
                                 blist:ukeysort(N, Size, Binary1),
                                 blist:ukeysort(N, Size, Binary2))).

prop_ukeysort_3() ->
     ?FORALL(
        {N, Size, Binary},
        key_list(),
        from_key_list(
          lists:ukeysort(N, to_key_list(Binary, Size)))
        ==
            blist:ukeysort(N, Size, Binary)).

prop_umerge_1() ->
    ?FORALL(
       LList,
       list(list(byte())),
       lists:umerge([lists:usort(L) || L <- LList]) ==
        binary_to_list(
          blist:umerge([list_to_binary(lists:usort(L)) || L <- LList]))).

prop_umerge_2() ->
    ?FORALL(
       {List1, List2},
       {list(byte()), list(byte())},
       lists:umerge(lists:usort(List1), lists:usort(List2)) ==
        binary_to_list(
          blist:umerge(list_to_binary(lists:usort(List1)),
                            list_to_binary(lists:usort(List2))))).

prop_umerge_3_leq() ->
    ?FORALL(
       {List1, List2},
       {list(byte()), list(byte())},
       lists:umerge(fun leq/2, lists:usort(List1), lists:usort(List2)) ==
        binary_to_list(
          blist:umerge(fun leq/2,
                            list_to_binary(lists:usort(List1)),
                            list_to_binary(lists:usort(List2))))).

prop_umerge_3_geq() ->
    ?FORALL(
       {List1, List2},
       {list(byte()), list(byte())},
       lists:umerge(fun geq/2,
                    lists:usort(fun geq/2, List1),
                    lists:usort(fun geq/2, List2)) ==
        binary_to_list(
          blist:umerge(fun geq/2,
                            list_to_binary(lists:usort(fun geq/2, List1)),
                            list_to_binary(lists:usort(fun geq/2, List2))))).

prop_umerge3_3() ->
    ?FORALL(
       {List1, List2, List3},
       {list(byte()), list(byte()), list(byte())},
       lists:umerge3(lists:usort(List1),
                     lists:usort(List2),
                     lists:usort(List3)) ==
        binary_to_list(
          blist:umerge3(list_to_binary(lists:usort(List1)),
                             list_to_binary(lists:usort(List2)),
                             list_to_binary(lists:usort(List3))))).

prop_unzip_1() ->
     ?FORALL(
        Binary,
        parts_list(2),
        lists:unzip(to_key_list(Binary, 2)) ==
            list_to_tuple([binary_to_list(B) ||
                              B <- tuple_to_list(blist:unzip(Binary))])).


prop_unzi3_1() ->
     ?FORALL(
        Binary,
        parts_list(3),
        lists:unzip3(to_key_list(Binary, 3)) ==
            list_to_tuple([binary_to_list(B) ||
                              B <- tuple_to_list(blist:unzip3(Binary))])).

parts_list(Size) ->
    ?LET(Parts, non_neg_integer(), binary(Size * Parts)).

prop_usort_1() ->
    ?FORALL(
       List,
       list(byte()),
       lists:usort(List) ==
        binary_to_list(blist:usort(list_to_binary(List)))).

prop_usort_2_leq() ->
    ?FORALL(
       List,
       list(byte()),
       lists:usort(fun leq/2, List) ==
           binary_to_list(
             blist:usort(fun leq/2, list_to_binary(List)))).

prop_usort_2_geq() ->
    ?FORALL(
       List,
       list(byte()),
       lists:usort(fun geq/2, List) ==
           binary_to_list(
             blist:usort(fun geq/2, list_to_binary(List)))).

prop_zip_2() ->
    ?FORALL(
       List,
       list(byte()),
       lists:zip(List, lists:reverse(List)) ==
           to_key_list(blist:zip(list_to_binary(List),
                                      list_to_binary(lists:reverse(List))),
                       2)).

prop_zip3_3() ->
    ?FORALL(
       List,
       list(byte()),
       lists:zip3(List, List, lists:reverse(List)) ==
           to_key_list(blist:zip3(list_to_binary(List),
                                       list_to_binary(List),
                                       list_to_binary(lists:reverse(List))),
                       3)).


prop_zipwith_3_tuple() ->
    ?FORALL(
       List,
       list(byte()),
       lists:zipwith(fun(X, Y) -> {X, Y} end,
                     List,
                     lists:reverse(List)) ==
           to_key_list(
             blist:zipwith(fun(X, Y) -> <<X, Y>> end,
                                list_to_binary(List),
                                list_to_binary(lists:reverse(List))),
             2)).

prop_zipwith3_4_tuple() ->
    ?FORALL(
       List,
       list(byte()),
       lists:zipwith3(fun(X, Y, Z) -> {X, Y, Z} end,
                      List,
                      List,
                      lists:reverse(List)) ==
           to_key_list(
             blist:zipwith3(fun(X, Y, Z) -> <<X, Y, Z>> end,
                                 list_to_binary(List),
                                 list_to_binary(List),
                                 list_to_binary(lists:reverse(List))),
             3)).
