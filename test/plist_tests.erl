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
%%    These tests are to ensure internal consistency in plist and
%%    basic functionality.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(plist_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

-define(EXCEMPT_SPECS, [{add, 4}, {delete, 3}, {new, 2}]).

-include("jhn_stdlib_proper.hrl").

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

prop_new_2() ->
    ?FORALL(
       List,
       list(),
       begin
           PList = plist:new(List, lists:reverse(List)),
           lists:all(fun(K) -> lists:keymember(K, 1, PList) end, List)
       end).

