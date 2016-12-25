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
                              {binary, part, 2},
                              {binary, part, 3},
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
                              digraph,
                              digraph_utils,
                              {epp, encoding_to_string, 1},
                              {epp, format_error, 1},
                              erl_anno,
                              {erl_ddll, format_error, 1},
                              erl_eval,
                              erl_expand_records,
                              erl_internal,
                              {erl_lint, format_error, 1},
                              {erl_lint, is_guard_test, 1},
                              {erl_lint, module, 1},
                              erl_parse,
                              erl_pp,
                              erl_scan,
                              {erl_tar, format_error, 1},
                              {erlang, abs, 1},
                              {erlang, adler32, 1},
                              {erlang, adler32, 2},
                              {erlang, adler32_combine, 3},
                              {erlang, append_element, 2},
                              {erlang, atom_to_binary, 2},
                              {erlang, atom_to_list, 1},
                              {erlang, binary_part, 2},
                              {erlang, binary_part, 3},
                              {erlang, binary_to_atom, 2},
                              {erlang, binary_to_float, 1},
                              {erlang, binary_to_integer, 1},
                              {erlang, binary_to_integer, 2},
                              {erlang, binary_to_list, 1},
                              {erlang, binary_to_list, 3},
                              {erlang, bitsring_to_list, 1},
                              {erlang, binary_to_term, 1},
                              {erlang, binary_to_term, 2},
                              {erlang, bit_size, 1},
                              {erlang, byte_size, 1},
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
                              {erlang, list_to_pid, 1},
                              {erlang, list_to_tuple, 1},
                              {erlang, make_tuple, 2},
                              {erlang, make_tuple, 3},
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
                              {erlang, pid_to_list, 1},
                              {erlang, ref_to_list, 1},
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
                              {etc, is_compiled_ms, 1},
                              dict,
                              {file, format_error, 1},
                              {filename, extension, 1},
                              gb_sets,
                              gb_trees,
                              {gen_sctp, error_string, 1},
                              {inet, format_error, 1},
                              {inet, ntoa, 1},
                              {inet, parse_address, 1},
                              {inet, parse_ipv4_address, 1},
                              {inet, parse_ipv4strict_address, 1},
                              {inet, parse_ipv6_address, 1},
                              {inet, parse_ipv6strict_address, 1},
                              {inet, parse_strict_address, 1},
                              io_lib,
                              {lib, nonl, 1},
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
                              {yecc, format_error, 1},
                              {zlib, compress, 2},
                              {zlib, gunzip, 2},
                              {zlib, gzip, 2},
                              {zlib, uncompress, 2},
                              {zlib, zip, 2},
                              {zlib, unzip, 2}]).

-record(state, {pure = sets:new() :: sets:set(mfa()),
                module :: module(),
                exports = sets:new() :: sets:set({atom(), arity()}),
                imports = sets:new() :: sets:set(mfa()),
                functions = sets:new() :: sets:set({atom(), arity()}),
                no_auto_imports = sets:new() :: sets:set(mfa()),
                tree :: erl_syntax:syntaxTree(),
                node = false :: erl_syntax:syntaxTree()|false}).

-spec parse_transform(Forms::[erl_syntax:syntaxTree()], proplists:proplist()) -> [erl_syntax:syntaxTree()].
parse_transform(Forms, _Options) ->
    try erl_syntax_lib:analyze_forms(Forms) of
       AF ->
           State = #state{pure = sets:from_list(lists:filter(fun(MFA) -> is_atom(MFA) orelse not is_pure(MFA) end,
                                                             ?TRANSFORM_FUNCTIONS)),
                          module = gv(module, AF),
                          exports = sets:from_list(gl(exports, AF)),
                          imports = sets:from_list(gl(imports, AF)),
                          functions = sets:from_list(gl(functions, AF)),
                          no_auto_imports = sets:from_list(get_no_auto_import(AF))},
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

-spec gv(K::term(), L::list(), D::term()) -> term().
gv(K, L, D) ->
    case lists:keyfind(K, 1, L) of
        {ok, V} -> V;
        _ -> D
    end.

-spec gv(K::term(), L::list()) -> undefined | term().
gv(K, L) -> gv(K, L, undefined).

-spec gl(K::term(), L::list()) -> [] | term().
gl(K, L) -> gv(K, L, []).

-spec ga(K::term(), L::list()) -> list().
ga(K, L) -> [V || {Key, V} <- L, Key =:= K].

-spec get_no_auto_import(AF::list()) -> [mfa()].
get_no_auto_import(AF) -> lists:flatten(ga(no_auto_import, ga(compile, gl(attributes, AF)))).

-spec transform(State::#state{}) -> #state{}.
transform(#state{tree = Tree} = State) ->
    case erl_syntax_lib:mapfold(fun(T, F) ->
                                    case const_transform(State#state{node = T}) of
                                        #state{node = false} -> {T, F};
                                        #state{node = Node} = S ->
                                            {erl_syntax_lib:map(fun(N) -> erl_syntax:copy_pos(T, N) end, Node), S}
                                    end
                                end, false, Tree) of
        {_, false} -> State;
        {NewTree, NewState} -> transform(NewState#state{tree = NewTree})
    end.

-spec const_transform(State::#state{}) -> #state{}.
const_transform(#state{node = Node} = State) ->
    case erl_syntax:type(Node) of
        application ->
            case erl_syntax_lib:analyze_application(Node) of
                {M, {F, A}} ->
                    Fs = State#state.pure,
                    State#state{node = is_pure({M, F, A}, Fs) andalso
                                           call(Fs, M, F, erl_syntax:application_arguments(Node))};
                _ -> State#state{node = false}
            end;
        infix_expr ->
            O = erl_syntax:operator_name(erl_syntax:infix_expr_operator(Node)),
            State#state{node = not erl_internal:send_op(O, 2) andalso
                                   call(O, [erl_syntax:infix_expr_left(Node), erl_syntax:infix_expr_right(Node)])};
        prefix_expr -> State#state{node = call(erl_syntax:operator_name(erl_syntax:prefix_expr_operator(Node)),
                                               [erl_syntax:prefix_expr_argument(Node)])};
        _ -> State#state{node = false}
    end.

-spec call(Fs::sets:set(mfa()), Mod::module(), Fun::atom(), Args::list()) -> false | erl_syntax:syntaxTree().
call(_Fs, Mod, Fun, []) -> try_call(Mod, Fun);
call(Fs, Mod, Fun, Args) ->
    lists:all(fun(A) -> erl_syntax:is_literal(A) orelse is_pure_fun(A, Fs) end, Args) andalso try_call(Mod, Fun, Args).

-spec call(Fun::atom(), Args::list()) -> false | erl_syntax:syntaxTree().
call(Fun, []) -> try_call(erlang, Fun);
call(Fun, Args) ->
    lists:all(fun erl_syntax:is_literal/1, Args) andalso try_call(erlang, Fun, Args).

-spec try_call(Mod::module(), Fun::atom()) -> false | erl_syntax:syntaxTree().
try_call(Mod, Fun) ->
    try Mod:Fun() of
        R -> erl_syntax:abstract(R)
    catch _:_ -> false
    end.

-spec try_call(Mod::module(), Fun::atom(), Args::list()) -> false | erl_syntax:syntaxTree().
try_call(Mod, Fun, Args) ->
    A = lists:map(fun concrete/1, Args),
    try apply(Mod, Fun, A) of
        R -> erl_syntax:abstract(R)
    catch _:_ -> false
    end.

-spec is_pure(F::mfa()|{module(), {atom(), arity()}}|{atom(), arity()}|term(), Fs::sets:set(mfa())) -> boolean().
is_pure({_, module_info, A}, _) when A =:= 0; A =:= 1 -> false;
is_pure({M, F, A} = MFA, Fs) when is_atom(M), is_atom(F), is_integer(A) ->
    is_pure(MFA) orelse sets:is_element(M, Fs) orelse sets:is_element(MFA, Fs);
is_pure({M, {F, A}}, Fs) -> is_pure({M, F, A}, Fs);
is_pure({F, A}, Fs) -> is_pure({erlang, F, A}, Fs);
is_pure(_, _) -> false.

is_pure({M, F, A}) -> erl_bifs:is_pure(M, F, A).

-spec is_pure_fun(A::erl_syntax:syntaxTree(), Fs::sets:set(mfa())) -> false | erl_syntax:syntaxTree().
is_pure_fun(A, Fs) ->
    erl_syntax:type(A) =:= implicit_fun andalso try is_pure(erl_syntax_lib:analyze_implicit_fun(A), Fs)
                                                catch throw:syntax_error -> false
                                                end.

-spec concrete(Node::erl_syntax:syntaxTree()) -> term().
concrete(Node) ->
    case erl_syntax:type(Node) of
        implicit_fun ->
            {M, {F, A}} = erl_syntax_lib:analyze_implicit_fun(Node),
            fun M:F/A;
        _ -> erl_syntax:concrete(Node)
    end.
