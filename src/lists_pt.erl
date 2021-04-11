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

-module(lists_pt).

-export([parse_transform/2]).

-import(erl_syntax, [set_pos/2]).

-record(state, {verbose :: boolean(),
                file = "" :: string(),
                module :: module(),
                imports = sets:new() :: sets:set(mfa()),
                functions = sets:new() :: sets:set({atom(), arity()}),
                tree :: erl_syntax:syntaxTree(),
                node = false :: erl_syntax:syntaxTree()|false}).

-spec parse_transform(Forms::[erl_syntax:syntaxTree()], proplists:proplist()) -> [erl_syntax:syntaxTree()].
parse_transform(Forms, Options) ->
    try erl_syntax_lib:analyze_forms(Forms) of
        AF ->
            Fs = sets:from_list(gl(functions, AF)),
            {NewForms, _} = lists:mapfoldl(fun(T, S) ->
                                               State = #state{tree = Tree} = transform(S#state{tree = T}),
                                               {erl_syntax:revert(Tree), State}
                                           end,
                                           #state{verbose = proplists:get_bool(verbose, Options),
                                                  module = gv(module, AF),
                                                  imports = get_imports(AF, Fs),
                                                  functions = Fs},
                                           Forms),
            NewForms
    catch
        C:E ->
            io:fwrite(standard_error,
                      ?MODULE_STRING ": error erl_syntax_lib:analyze_forms/1 {~p:~p}, see below.~n",
                      [C, E]),
            Forms
    end.

-spec transform(State::#state{}) -> #state{}.
transform(#state{tree = Tree} = State) ->
    case erl_syntax:type(Tree) =:= attribute andalso erl_syntax_lib:analyze_attribute(Tree) of
        {file, {F, _}} -> State#state{file = F};
        _ -> case erl_syntax_lib:mapfold(fun(T, F) ->
                                             case transform(State#state{node = T}, erl_syntax:type(T)) of
                                                 #state{node = false} -> {T, F};
                                                 #state{node = Node} = S -> {erl_syntax:copy_pos(T, Node), S}
                                             end
                                         end, false, Tree) of
                 {_, false} -> State;
                 {NewTree, NewState} -> transform(NewState#state{tree = NewTree})
             end
    end.

-spec transform(State::#state{}, atom()) -> #state{}.
transform(#state{node = Node} = State, application) ->
    case erl_syntax_lib:analyze_application(Node) of
        {lists, {F, A}} -> transform(State, F, A);
        {F, A} = FA -> case sets:is_element(FA, State#state.imports) of
                           true -> transform(State, F, A);
                           _ -> State#state{node = false}
                       end;
        _ -> State#state{node = false}
    end;
transform(#state{node = Node, verbose = true} = State, attribute) ->
    S = State#state{node = false},
    case erl_syntax_lib:analyze_attribute(Node) of
        {file, {F, _}} -> S#state{file = F};
        _ -> S
    end;
transform(#state{} = State, _) -> State#state{node = false}.

-spec transform(State::#state{}, atom(), arity()) -> #state{}.
transform(#state{node = Node} = State, member, 2) ->
    [E, L] = erl_syntax:application_arguments(Node),
    State#state{node = case transform_member(E, L) of
                           false -> false;
                           N ->
                               State#state.verbose andalso io:fwrite(?MODULE_STRING ": ~s ~B member/2~n",
                                                                     [State#state.file, erl_syntax:get_pos(Node)]),
                               N
                       end};
transform(#state{node = Node} = State, map, 2) ->
    [F, L] = erl_syntax:application_arguments(Node),
    State#state{node = case transform_map(Node, F, L) of
                           false -> false;
                           N ->
                               State#state.verbose andalso io:fwrite(?MODULE_STRING ": ~s ~B map/2~n",
                                                                     [State#state.file, erl_syntax:get_pos(Node)]),
                               N
                       end};
transform(State, _F, _A) -> State#state{node = false}.

-spec transform_member(M::erl_syntax:syntaxTree(), L::erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree()|false.
transform_member(M, N) ->
    case erl_syntax:type(N) of
        nil -> erl_syntax:copy_pos(M, erl_syntax:atom(false));
        list ->
            erl_syntax:type(M) =:= variable andalso erl_syntax:is_literal(N) andalso
                begin
                Oe = erl_syntax:operator('=:='),
                V = erl_syntax:variable(erl_syntax:variable_name(M)),
                {[H|T], _} = lists:foldl(fun(E, {L, S} = A) ->
                                             T = erl_syntax:concrete(E),
                                             case sets:is_element(T, S) of
                                                 true -> A;
                                                 _false ->
                                                     P = erl_syntax:get_pos(E),
                                                     {[set_pos(erl_syntax:infix_expr(set_pos(V, P),
                                                                                     set_pos(Oe, P),
                                                                                     E),
                                                               P)|L],
                                                      sets:add_element(T, S)}
                                             end
                                         end, {[], sets:new()}, erl_syntax:list_elements(N)),
                Oo = erl_syntax:operator('orelse'),
                lists:foldl(fun(E, A) ->
                                P = erl_syntax:get_pos(E),
                                set_pos(erl_syntax:infix_expr(E, set_pos(Oo, P), A), P)
                            end, H, T)
                end;
        _ -> false
    end.

-spec transform_map(Node::erl_syntax:syntaxTree(), F::erl_syntax:syntaxTree(), L::erl_syntax:syntaxTree()) ->
          erl_syntax:syntaxTree()|false.
transform_map(Node, F, L) ->
    case erl_syntax:type(F) of
        variable -> list_comp(Node, F, L, F, F);
        fun_expr ->
            case erl_syntax:fun_expr_clauses(F) of
                [C] ->
                    case erl_syntax:clause_guard(C) =:= none andalso erl_syntax:clause_patterns(C) of
                        [P] ->
                            PT = erl_syntax:type(P),
                            case (PT =:= variable orelse PT =:= underscore) andalso erl_syntax:clause_body(C) of
                                [_|_] = B ->
                                    V = erl_syntax:copy_pos(L,
                                                            if
                                                                PT =:= underscore -> erl_syntax:underscore();
                                                                true -> erl_syntax:variable(erl_syntax:variable_name(P))
                                                            end),
                                    erl_syntax:list_comp(case B of
                                                             [E] -> E;
                                                             _ -> erl_syntax:copy_pos(F, erl_syntax:block_expr(B))
                                                         end,
                                                         [erl_syntax:copy_pos(L, erl_syntax:generator(V, L))]);
                                _ -> false
                            end;
                        _ -> false
                    end;
                _ -> false
            end;
        implicit_fun ->
            FN = erl_syntax:implicit_fun_name(F),
            case erl_syntax:type(FN) of
                arity_qualifier ->
                    A = erl_syntax:arity_qualifier_argument(FN),
                    case erl_syntax:type(A) =:= integer andalso erl_syntax:integer_value(A) of
                        1 ->
                            B = erl_syntax:arity_qualifier_body(FN),
                            case erl_syntax:type(B) of
                                atom -> list_comp(Node, F, L, A, B);
                                _ -> false
                            end;
                        _ -> false
                    end;
                module_qualifier ->
                    AA = erl_syntax:module_qualifier_body(FN),
                    A = erl_syntax:arity_qualifier_argument(AA),
                    case erl_syntax:type(A) =:= integer andalso erl_syntax:integer_value(A) of
                        1 ->
                            B = erl_syntax:arity_qualifier_body(AA),
                            case erl_syntax:type(B) of
                                atom ->
                                    M = erl_syntax:module_qualifier_argument(FN),
                                    list_comp(Node, F, L, A,
                                              erl_syntax:copy_pos(M, erl_syntax:module_qualifier(M, B)));
                                _ -> false
                            end;
                        _ -> false
                    end;
                _ -> false
            end;
        _ -> false
    end.

-spec list_comp(Node::erl_syntax:syntaxTree(), F::erl_syntax:syntaxTree(), L::erl_syntax:syntaxTree(),
                A::erl_syntax:syntaxTree(), Q::erl_syntax:syntaxTree()) ->
          erl_syntax:syntaxTree().
list_comp(Node, F, L, A, Q) ->
    V = erl_syntax:variable(erl_syntax_lib:new_variable_name(erl_syntax_lib:variables(Node))),
    erl_syntax:list_comp(erl_syntax:copy_pos(F, erl_syntax:application(Q, [erl_syntax:copy_pos(A, V)])),
                         [erl_syntax:copy_pos(L, erl_syntax:generator(erl_syntax:copy_pos(L, V), L))]).

-spec gv(K::term(), L::list(), D::term()) -> term().
gv(K, L, D) ->
    case lists:keyfind(K, 1, L) of
        {_, V} -> V;
        _ -> D
    end.

-spec gv(K::term(), L::list()) -> undefined | term().
gv(K, L) -> gv(K, L, undefined).

-spec gl(K::term(), L::list()) -> [] | term().
gl(K, L) -> gv(K, L, []).

-spec ga(K::term(), L::list()) -> list().
ga(K, L) -> [V || {Key, V} <- L, Key =:= K].

-spec get_imports(AF::list(), Fs::sets:set({atom(), arity()})) -> sets:set({atom(), arity()}).
get_imports(AF, Fs) ->
    lists:foldl(fun(IFs, S) ->
                    lists:foldl(fun(FA, A) ->
                                    case sets:is_element(FA, Fs) of
                                        false -> sets:add_element(FA, A);
                                        _true -> A
                                    end
                                end, S, IFs)
                end, sets:new(), ga(lists, gl(imports, AF))).
