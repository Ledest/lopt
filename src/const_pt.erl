%%% Copyright 2016-2024 Oleksandr Chumachenko <ledest@gmail.com>
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

-module(const_pt).

-export([parse_transform/2]).

-define(TRANSFORM_FUNCTIONS, [array,
                              {asn1rt, utf8_binary_to_list, 1},
                              {asn1rt, utf8_list_to_binary, 1},
                              base64,
                              {beam_lib, build_module, 1},
                              {beam_lib, format_error, 1},
                              {binary, at, 2},
                              {binary, bin_to_list, 1},
                              {binary, bin_to_list, 2},
                              {binary, bin_to_list, 3},
                              {binary, copy, 1},
                              {binary, copy, 2},
                              {binary, decode_unsigned, 1},
                              {binary, decode_unsigned, 2},
                              {binary, encode_unsigned, 1},
                              {binary, encode_unsigned, 2},
                              {binary, first, 1},
                              {binary, last, 1},
                              {binary, list_to_bin, 1},
                              {binary, longest_common_prefix, 1},
                              {binary, longest_common_suffix, 1},
                              {binary, match, 2},
                              {binary, match, 3},
                              {binary, matches, 3},
                              {binary, part, 2},
                              {binary, part, 3},
                              {binary, split, 2},
                              {binary, split, 3},
                              {calendar, date_to_gregorian_days, 1},
                              {calendar, date_to_gregorian_days, 3},
                              {calendar, datetime_to_gregorian_seconds, 1},
                              {calendar, day_of_the_week, 1},
                              {calendar, day_of_the_week, 3},
                              {calendar, gregorian_days_to_date, 1},
                              {calendar, gregorian_seconds_to_datetime, 1},
                              {calendar, is_leap_year, 1},
                              {calendar, iso_week_number, 1},
                              {calendar, last_day_of_the_month, 2},
                              {calendar, now_to_datetime, 1},
                              {calendar, seconds_to_daystime, 1},
                              {calendar, seconds_to_time, 1},
                              {calendar, time_difference, 2},
                              {calendar, time_to_seconds, 1},
                              {calendar, valid_date, 1},
                              {calendar, valid_date, 3},
                              {code, objfile_extension, 0},
                              {compile, format_error, 1},
                              {compile, noenv_forms, 2},
                              {compile, noenv_output_generated, 1},
                              {compile, output_generated, 1},
                              dict,
                              {epp, encoding_to_string, 1},
                              {epp, format_error, 1},
                              erl_anno,
                              erl_bits,
                              {erl_ddll, format_error, 1},
                              erl_eval,
                              erl_expand_records,
                              erl_internal,
                              {erl_lint, format_error, 1},
                              {erl_lint, is_guard_test, 1},
                              {erl_lint, module, 1},
                              erl_parse,
                              erl_posix_msg,
                              erl_pp,
                              erl_scan,
                              {erl_syntax, eof_marker, 0},
                              {erl_syntax, fun_type, 0},
                              {erl_syntax, map_type, 0},
                              {erl_syntax, nil, 0},
                              {erl_syntax, tuple_type, 0},
                              {erl_syntax, underscore, 0},
                              {erl_tar, format_error, 1},
                              {erlang, abs, 1},
                              {erlang, adler32, 1},
                              {erlang, adler32, 2},
                              {erlang, adler32_combine, 3},
                              {erlang, append_element, 2},
                              {erlang, atom_to_binary, 1},
                              {erlang, atom_to_binary, 2},
                              {erlang, atom_to_list, 1},
                              {erlang, binary_part, 2},
                              {erlang, binary_part, 3},
                              {erlang, binary_to_atom, 1},
                              {erlang, binary_to_atom, 2},
                              {erlang, binary_to_float, 1},
                              {erlang, binary_to_integer, 1},
                              {erlang, binary_to_integer, 2},
                              {erlang, binary_to_list, 1},
                              {erlang, binary_to_list, 3},
                              {erlang, bitstring_to_list, 1},
                              {erlang, binary_to_term, 1},
                              {erlang, binary_to_term, 2},
                              {erlang, bit_size, 1},
                              {erlang, byte_size, 1},
                              {erlang, ceil, 1},
                              {erlang, convert_time_unit, 3},
                              {erlang, crc32, 1},
                              {erlang, crc32, 2},
                              {erlang, crc32_combine, 3},
                              {erlang, decode_packet, 3},
                              {erlang, delete_element, 2},
                              {erlang, element, 2},
                              {erlang, external_size, 1},
                              {erlang, external_size, 2},
                              {erlang, float, 1},
                              {erlang, float_to_binary, 1},
                              {erlang, float_to_binary, 2},
                              {erlang, float_to_list, 1},
                              {erlang, float_to_list, 2},
                              {erlang, floor, 1},
                              {erlang, fun_info, 1},
                              {erlang, fun_info, 2},
                              {erlang, fun_to_list, 1},
                              {erlang, hash, 2},
                              {erlang, hd, 1},
                              {erlang, insert_element, 3},
                              {erlang, integer_to_binary, 1},
                              {erlang, integer_to_binary, 2},
                              {erlang, integer_to_list, 1},
                              {erlang, integer_to_list, 2},
                              {erlang, iolist_to_binary, 1},
                              {erlang, iolist_size, 1},
                              {erlang, is_map_key, 2},
                              {erlang, is_atom, 1},
                              {erlang, is_binary, 1},
                              {erlang, is_bitstring, 1},
                              {erlang, is_boolean, 1},
                              {erlang, is_float, 1},
                              {erlang, is_function, 1},
                              {erlang, is_function, 2},
                              {erlang, is_integer, 1},
                              {erlang, is_list, 1},
                              {erlang, is_map, 1},
                              {erlang, is_number, 1},
                              {erlang, is_pid, 1},
                              {erlang, is_port, 1},
                              {erlang, is_record, 2},
                              {erlang, is_record, 3},
                              {erlang, is_reference, 1},
                              {erlang, is_tuple, 1},
                              {erlang, length, 1},
                              {erlang, list_to_atom, 1},
                              {erlang, list_to_binary, 1},
                              {erlang, list_to_bitstring, 1},
                              {erlang, list_to_float, 1},
                              {erlang, list_to_integer, 1},
                              {erlang, list_to_integer, 2},
                              {erlang, list_to_tuple, 1},
                              {erlang, make_fun, 3},
                              {erlang, make_tuple, 2},
                              {erlang, make_tuple, 3},
                              {erlang, map_get, 2},
                              {erlang, map_size, 1},
                              {erlang, max, 2},
                              {erlang, md5, 1},
                              {erlang, md5_final, 1},
                              {erlang, md5_init, 0},
                              {erlang, md5_update, 2},
                              {erlang, min, 2},
                              {erlang, phash, 2},
                              {erlang, phash2, 1},
                              {erlang, phash2, 2},
                              {erlang, round, 1},
                              {erlang, setelement, 3},
                              {erlang, size, 1},
                              {erlang, split_binary, 2},
                              {erlang, term_to_binary, 1},
                              {erlang, term_to_binary, 2},
                              {erlang, tl, 1},
                              {erlang, trunc, 1},
                              {erlang, tuple_size, 1},
                              {erlang, tuple_to_list, 1},
                              {ets, fun2ms, 1},
                              {ets, is_compiled_ms, 1},
                              {file, format_error, 1},
                              {filename, extension, 1},
                              gb_sets,
                              gb_trees,
                              {gen_sctp, error_string, 1},
                              {inet, format_error, 1},
                              {inet, ipv4_mapped_ipv6_address, 1},
                              {inet, ntoa, 1},
                              {inet, parse_address, 1},
                              {inet, parse_ipv4_address, 1},
                              {inet, parse_ipv4strict_address, 1},
                              {inet, parse_ipv6_address, 1},
                              {inet, parse_ipv6strict_address, 1},
                              {inet, parse_strict_address, 1},
                              io_lib,
                              {lib, nonl, 1},
                              {lib, error_message, 2},
                              lists,
                              maps,
                              math,
                              orddict,
                              ordsets,
                              {proc_lib, format, 1},
                              {proc_lib, format, 2},
                              {proc_lib, format, 3},
                              proplists,
                              %qlc,
                              queue,
                              re,
                              sets,
                              sofs,
                              string,
                              {supervisor, check_childspecs, 1},
                              {timer, hms, 3},
                              {timer, hours, 1},
                              {timer, minutes, 1},
                              {timer, now_diff, 2},
                              {timer, seconds, 1},
                              unicode,
                              {unicode_util, whitespace, 0},
                              uri_string,
                              {yecc, format_error, 1},
                              {zlib, compress, 1},
                              {zlib, gunzip, 1},
                              {zlib, gzip, 1},
                              {zlib, uncompress, 1},
                              {zlib, zip, 1},
                              {zlib, unzip, 1}]).

-record(state, {verbose :: boolean(),
                pure = sets:new() :: sets:set(mfa()),
                file = "" :: string(),
                module :: module(),
                exports = sets:new() :: sets:set({atom(), arity()}),
                imports = #{} :: #{ {atom(), arity()} => module()},
                functions = sets:new() :: sets:set({atom(), arity()}),
                tree :: erl_syntax:syntaxTree(),
                node = false :: erl_syntax:syntaxTree()|false}).

-spec parse_transform(Forms::[erl_syntax:syntaxTree()], proplists:proplist()) -> [erl_syntax:syntaxTree()].
parse_transform(Forms, Options) ->
    try erl_syntax_lib:analyze_forms(Forms) of
        AF ->
            Fs = sets:from_list(gl(functions, AF)),
            State = #state{verbose = proplists:get_bool(verbose, Options),
                           pure = sets:from_list(lists:filter(fun(MFA) -> is_atom(MFA) orelse not is_pure(MFA) end,
                                                              ?TRANSFORM_FUNCTIONS)),
                           module = gv(module, AF),
                           exports = sets:from_list(gl(exports, AF)),
                           imports = get_imports(AF, Fs),
                           functions = Fs},
            {NewForms, _} = lists:mapfoldl(fun(Tree, S) ->
                                               NewState = #state{tree = NewTree} = transform(S#state{tree = Tree}),
                                               {erl_syntax:revert(NewTree), NewState}
                                           end, State, Forms),
            NewForms
    catch
        C:E ->
            io:fwrite(standard_error,
                      ?MODULE_STRING ": error erl_syntax_lib:analyze_forms/1 {~p:~p}, see below.~n",
                      [C, E]),
            Forms
    end.

-spec gv(K::term(), L::list(), D) -> D | term().
gv(K, L, D) ->
    case lists:keyfind(K, 1, L) of
        {_, V} -> V;
        _false -> D
    end.

-spec gv(K::term(), L::list()) -> undefined | term().
gv(K, L) -> gv(K, L, undefined).

-spec gl(K::term(), L::list()) -> [] | term().
gl(K, L) -> gv(K, L, []).

-compile({inline, [gv/2]}).

-spec ga(K::term(), L::list()) -> list().
ga(K, L) -> [V || {Key, V} <- L, Key =:= K].

-spec get_no_auto_import(AF::list()) -> [mfa()].
get_no_auto_import(AF) -> lists:flatten(ga(no_auto_import, ga(compile, gl(attributes, AF)))).

-spec get_imports(AF::list(), Fs::sets:set({atom(), arity()})) -> #{{atom(), arity()} => module()}.
get_imports(AF, Fs) ->
    NAI = sets:from_list(get_no_auto_import(AF)),
    lists:foldl(fun({M, IFs}, D) ->
                    lists:foldl(fun(FA, A) ->
                                    case sets:is_element(FA, Fs) of
                                        false -> A#{FA => M};
                                        _true -> A
                                    end
                                end, D, IFs)
                end, #{},
                [{erlang, [{F, A} || {erlang, F, A} <- ?TRANSFORM_FUNCTIONS,
                                                       erl_internal:bif(F, A),
                                                       not sets:is_element({F, A}, NAI)]}|gl(imports, AF)]).

-compile({inline, [get_imports/2]}).

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
    State#state{node = case erl_syntax_lib:analyze_application(Node) of
                           {M, {F, A}} -> transform(State, M, F, A);
                           {F, A} = FA ->
                               case State#state.imports of
                                   #{FA := M} -> transform(State, M, F, A);
                                   _ -> false
                               end;
                           _ -> false
                       end};
transform(#state{node = Node} = State, infix_expr) ->
    O = erl_syntax:operator_name(erl_syntax:infix_expr_operator(Node)),
    State#state{node = not erl_internal:send_op(O, 2) andalso
                       copy_pos(Node,
                                call(State, O, [erl_syntax:infix_expr_left(Node), erl_syntax:infix_expr_right(Node)]))};
transform(#state{node = Node} = State, prefix_expr) ->
    State#state{node = copy_pos(Node, call(State, erl_syntax:operator_name(erl_syntax:prefix_expr_operator(Node)),
                                           [erl_syntax:prefix_expr_argument(Node)]))};
transform(#state{node = Node} = State, fun_expr) ->
    State#state{node = transform(State, fun_expr, erl_syntax:fun_expr_clauses(Node))};
transform(#state{} = State, _) -> State#state{node = false}.

-spec transform(State::#state{}, atom(), [erl_syntax:syntaxTree(),...]) -> erl_syntax:syntaxTree() | false.
transform(#state{node = Node} = State, fun_expr, [Clause]) ->
    erl_syntax:clause_guard(Clause) =:= none andalso transform(State, fun_expr, erl_syntax:fun_expr_arity(Node),
                                                               erl_syntax:clause_patterns(Clause),
                                                               erl_syntax:clause_body(Clause));
transform(#state{}, _, _) -> false.

-spec transform(State::#state{}, M::module(), F::atom(), A::arity()) -> erl_syntax:syntaxTree() | false.
transform(#state{node = Node, pure = P} = State, M, F, A) ->
    is_pure({M, F, A}, P) andalso copy_pos(Node, call(State, M, F, erl_syntax:application_arguments(Node))).

-spec transform(State::#state{}, atom(), A::non_neg_integer(),
                P::[erl_syntax:syntaxTree()], [erl_syntax:syntaxTree(),...]) ->
          erl_syntax:syntaxTree() | false.
transform(#state{}, fun_expr, 0, [], [B]) ->
    erl_syntax:type(B) =:= application andalso
        case erl_syntax_lib:analyze_application(B) of
            {M, {F, 0}} ->
                O = erl_syntax:application_operator(B),
                MB = erl_syntax:module_qualifier_body(O),
                implicit_fun(M, F, 0, O, MB, MB);
            {F, 0} -> erl_syntax:implicit_fun(none, erl_syntax:atom(F), erl_syntax:integer(0));
            _ -> false
        end;
transform(#state{}, fun_expr, A, P, [B]) ->
    case erl_syntax:type(B) of
        application -> case erl_syntax_lib:analyze_application(B) of
                           {M, {F, A}} ->
                               AA = erl_syntax:application_arguments(B),
                               compare_arguments(P, AA) andalso
                                   begin
                                   O = erl_syntax:application_operator(B),
                                   implicit_fun(M, F, A, O, erl_syntax:module_qualifier_body(O), hd(AA))
                                   end;
                           {F, A} -> compare_arguments(P, erl_syntax:application_arguments(B)) andalso
                                         erl_syntax:implicit_fun(none, erl_syntax:atom(F), erl_syntax:integer(A));
                           _ -> false
                       end;
        _ -> false
    end;
transform(#state{}, _, _, _, _) -> false.

-compile({inline, [transform/2, transform/5]}).

-spec implicit_fun(M::module(), F::atom(), A::arity(), O::erl_syntax:syntaxTree(), MB::erl_syntax:syntaxTree(),
                   A1::erl_syntax:syntaxTree()) ->
          erl_syntax:syntaxTree().
implicit_fun(M, F, A, O, MB, A1) ->
    erl_syntax:implicit_fun(erl_syntax:copy_pos(O, erl_syntax:atom(M)),
                            erl_syntax:copy_pos(MB, erl_syntax:atom(F)),
                            erl_syntax:copy_pos(A1, erl_syntax:integer(A))).

-spec call(State::#state{}, Mod::module(), Fun::atom(), Args::list()) -> erl_syntax:syntaxTree() | false.
call(State, Mod, Fun, []) -> try_call(State, Mod, Fun);
call(#state{pure = Fs} = State, Mod, Fun, Args) ->
    lists:all(fun(A) -> erl_syntax:is_literal(A) orelse is_pure_fun(A, Fs) end, Args) andalso
        try_call(State, Mod, Fun, Args).

-spec call(State::#state{}, Fun::atom(), Args::list()) -> false | erl_syntax:syntaxTree().
call(State, Fun, []) -> try_call(State, erlang, Fun);
call(State, Fun, Args) ->
    lists:all(fun erl_syntax:is_literal/1, Args) andalso try_call(State, erlang, Fun, Args).

-compile({inline, [call/4]}).

-spec try_call(S::#state{}, Mod::module(), Fun::atom()) -> false | erl_syntax:syntaxTree().
try_call(S, Mod, Fun) ->
    try Mod:Fun() of
        R ->
            verbose(S, " ~p:~p() => ~p~n", [Mod, Fun, R]),
            erl_syntax:abstract(R)
    catch
        error:R ->
            verbose(S, " ~p:~p() => erlang:error(~p)~n", [Mod, Fun, R]),
            erl_syntax:application(erl_syntax:atom(erlang), erl_syntax:atom(error), [erl_syntax:abstract(R)]);
        _:_ -> false
    end.

-spec try_call(S::#state{}, Mod::module(), Fun::atom(), Args::list()) -> false | erl_syntax:syntaxTree().
try_call(S, Mod, Fun, Args) ->
    A = [concrete(X) || X <- Args],
    try apply(Mod, Fun, A) of
        R ->
            verbose(S, "~p: ~p:~p(~ts) => ~p~n",
                    [erl_syntax:get_pos(hd(Args)), Mod, Fun,
                     string:join([io_lib:print(erl_syntax:concrete(X)) || X <- Args], ","), R]),
            erl_syntax:abstract(R)
    catch
        error:R ->
            verbose(S, "~p: ~p:~p(~ts) => erlang:error(~p)~n",
                    [erl_syntax:get_pos(hd(Args)), Mod, Fun,
                     string:join([io_lib:print(erl_syntax:concrete(X)) || X <- Args], ","), R]),
            erl_syntax:application(erl_syntax:atom(erlang), erl_syntax:atom(error), [erl_syntax:abstract(R)]);
        _:_ -> false
    end.

-spec is_pure(F::mfa()|{module(), {atom(), arity()}}|{atom(), arity()}|term(), Fs::sets:set(mfa())) -> boolean().
is_pure({_, module_info, A}, _) when A =:= 0; A =:= 1 -> false;
is_pure({M, F, A} = MFA, Fs) when is_atom(M), is_atom(F), is_integer(A), A >= 0 ->
    erl_bifs:is_pure(M, F, A) orelse sets:is_element(M, Fs) orelse sets:is_element(MFA, Fs);
is_pure({M, {F, A}}, Fs) -> is_pure({M, F, A}, Fs);
is_pure({F, A}, Fs) -> is_pure({erlang, F, A}, Fs);
is_pure(_, _) -> false.

-spec is_pure(mfa()) -> boolean().
is_pure({M, F, A}) -> erl_bifs:is_pure(M, F, A).

-spec is_pure_fun(A::erl_syntax:syntaxTree(), Fs::sets:set(mfa())) -> boolean().
is_pure_fun(A, Fs) -> is_pure_fun(A, Fs, erl_syntax:type(A)).

-spec is_pure_fun(A::erl_syntax:syntaxTree(), Fs::sets:set(mfa()), atom()) -> boolean().
is_pure_fun(A, Fs, implicit_fun) ->
    try erl_syntax_lib:analyze_implicit_fun(A) of
        MFA -> is_pure(MFA, Fs)
    catch
        throw:syntax_error -> false
    end;
is_pure_fun(_, _, _) -> false.

-compile({inline, [is_pure_fun/2, is_pure_fun/3]}).

-spec concrete(Node::erl_syntax:syntaxTree()) -> term().
concrete(Node) -> concrete(Node, erl_syntax:type(Node)).

-spec concrete(Node::erl_syntax:syntaxTree(), atom()) -> term().
concrete(Node, implicit_fun) ->
    {M, {F, A}} = erl_syntax_lib:analyze_implicit_fun(Node),
    fun M:F/A;
concrete(Node, _) -> erl_syntax:concrete(Node).

-compile({inline, [concrete/2]}).

-spec compare_arguments([erl_syntax:syntaxTree()], [erl_syntax:syntaxTree()]) -> boolean().
compare_arguments([A|X], [B|Y]) ->
    erl_syntax:type(A) =:= variable andalso erl_syntax:type(B) =:= variable
         andalso erl_syntax:variable_name(A) =:= erl_syntax:variable_name(B)
         andalso compare_arguments(X, Y);
compare_arguments([], []) -> true;
compare_arguments(_, _) -> false.

-spec copy_pos(Source::erl_syntax:syntaxTree(), Tree::erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree();
              (Source::erl_syntax:syntaxTree(), false) -> false.
copy_pos(_, false) -> false;
copy_pos(Source, Tree) -> erl_syntax_lib:map(fun(N) -> erl_syntax:copy_pos(Source, N) end, Tree).

verbose(#state{verbose = true, file = F}, Fmt, Args) ->
    io:put_chars([?MODULE_STRING ": ", F, $:]),
    io:fwrite(Fmt, Args);
verbose(_, _, _) -> ok.
