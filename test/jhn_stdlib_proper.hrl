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

%%
%% General purpose utility functions to work with eunit/proper combination.
%%

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

'$properties'() ->
    [{Name, list_to_atom("prop_" ++ Name)}  ||
        "prop_" ++ Name <-
            [atom_to_list(F) || {F, 0} <- ?MODULE:module_info(exports)]].

proper_test_() ->
    {timeout, 60,
     [{Name, ?_assert(proper:quickcheck(?MODULE:F()))} ||
         {Name, F} <- '$properties'()]}.






