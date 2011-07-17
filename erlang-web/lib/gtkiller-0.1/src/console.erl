-module(console).

-export([log/1, log/2]).

log(Arg) when is_atom(Arg) ->
    log([atom_to_list(Arg)]);
log(Args) ->
    log(Args, 'DEBUG').
%log(Args, Mode) ->
%    apply(io, format, ["~-15w " ++ lists:flatmap(fun(_) -> "~p " end, Args) ++ "~n", [Mode] ++ Args]).
log(Args, Mode) ->
    apply(e_olup_logger, log, [Mode, lists:flatmap(fun(_) -> "~p " end, Args) ++ "~n", Args]).