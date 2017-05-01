%%% Copyright 2017 Oleksandr Chumachenko <ledest@gmail.com>
%%%
%%% This file is part of LOpt.
%%%
%%% LOpt is free software: you can redistribute it and/or modify it
%%% under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% LOpt is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%%% See the GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with LOpt. If not, see <http://www.gnu.org/licenses/>.

-module(lopt_pt).

-export([parse_transform/2]).

-spec parse_transform(Forms::[erl_syntax:syntaxTree()], proplists:proplist()) -> [erl_syntax:syntaxTree()].
parse_transform(Forms, Options) ->
    lists:foldl(fun(M, A) -> M:parse_transform(A, Options) end, Forms, [lists_pt, const_pt, inline_pt]).
