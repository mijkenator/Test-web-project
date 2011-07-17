-module(controller_base).

-behaviour(controller).

% Behaviour.
-export([handle/1]).
% Controller JSON-actions.
-export([default/0, default/1]).
% Utils.
-export([get_controller/0, get_method/0, extract_from_command/1]).

-include("constants.hrl").


%%%
%%% Behaviour.
%%%

%%
handle(_Argument) ->
    controller:handle(get_controller()).

%%%
%%% Controller JSON-actions.
%%%

%%
default()->
    utils_error:insert(?ERROR_ACTION_FAILD),
    default(error).

default(error)->
    [{?JSON_ERRORS, [utils_error:format(E) || E <- utils_error:select()]}].

%%%
%%% Utils.
%%%

%%
get_controller() ->
    wpart:fget("__controller").

%%
%% XXX: Ugly hack, but didn't find out something best.
get_method() ->
    case wpart:fget("post") of
        undefined ->
            ?METHOD_GET;
        _ ->
            ?METHOD_POST
    end.

%%
%%
extract_from_command(Fields) ->
    Command = wpart:fget(?KEY_COMMAND_JSON),
    [jsonutils:get_attribute(Command, utils:to_binary(X)) || X <- Fields].