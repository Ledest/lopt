%%% Copyright 2016 Oleksandr Chumachenko <ledest@gmail.com>
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

-module(inline_pt).

-export([parse_transform/2]).

-record(state, {forms :: list(), verbose :: boolean()}).

parse_transform(Forms, Options) ->
    case proplists:get_bool(inline, Options) of
        true -> inline_transform(#state{forms = Forms, verbose = proplists:get_bool(verbose, Options)});
        _ -> Forms
    end.

inline_transform(#state{forms = Forms} = S) ->
    case lists:member(export_all, CA = gav(compile, gv(attributes, AF = erl_syntax_lib:analyze_forms(Forms)))) of
        true -> Forms;
        false -> case (gv(functions, AF) -- gv(exports, AF)) -- gav(inline, CA) of
                     [_|_] = Functions ->
                         case maps:fold(fun(F, false, A) -> [F|A];
                                           (_, _, A) -> A
                                        end, [],
                                        lists:foldl(fun(Tree, Fs) ->
                                                        erl_syntax_lib:fold(fun count_func_calls/2, Fs, Tree)
                                                    end,
                                                    lists:foldl(fun(F, A) -> A#{F => true} end, #{}, Functions),
                                                    Forms)) of
                             [] -> Forms;
                             IF ->
                                 IFS = lists:sort(IF),
                                 S#state.verbose andalso
                                     io:fwrite(?MODULE_STRING ": module=~p, functions=~p~n", [gv(module, AF), IFS]),
                                 lists:foldr(fun({eof, L} = E, A) ->
                                                 [{attribute, L, compile, [{inline, IFS}]}, E|A];
                                                (E, A) -> [E|A]
                                             end, [], Forms)
                         end;
                     _ -> Forms
                 end

    end.

count_func_calls(Node, Acc) ->
    case analyze(Node) of
        {Name, Arity} = F when is_atom(Name), is_integer(Arity) ->
            case Acc of
                #{F := true} -> Acc#{F => false};
                #{F := _} -> maps:remove(F, Acc);
                _ -> Acc
            end;
        _ -> Acc
    end.

analyze(Node) ->
    case erl_syntax:type(Node) of
        application -> erl_syntax_lib:analyze_application(Node);
        implicit_fun -> erl_syntax_lib:analyze_implicit_fun(Node);
        _ -> false
    end.

gv(K, L) ->
    case lists:keyfind(K, 1, L) of
        {_, V} -> V;
        _ -> []
    end.

gav(Key, L) -> lists:flatten([V || {K, V} <- L, K =:= Key]).

-compile({inline, [inline_transform/1, analyze/1, count_func_calls/2]}).
