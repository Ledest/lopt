-module(lopt_pt).

-export([parse_transform/2]).

-spec parse_transform(Forms::[erl_syntax:syntaxTree()], proplists:proplist()) -> [erl_syntax:syntaxTree()].
parse_transform(Forms, Options) ->
    lists:foldl(fun(M, A) -> M:parse_transform(A, Options) end, Forms, [const_pt, inline_pt]).
