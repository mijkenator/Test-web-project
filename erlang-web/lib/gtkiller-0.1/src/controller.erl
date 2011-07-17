-module(controller).

-export([behaviour_info/1]).
-export([handle/1]).

-include("utils_controller_annotations.hrl").
-include("constants.hrl").


%%
behaviour_info(callbacks) ->
    [{handle, 1}];
behaviour_info(_) ->
    undefined.

%%%
%%% Request handler.
%%%

%%
?PROCESS_REQUEST({}).     % Before.
?PROCESS_RESPONSE({}).    % After.
handle(Controller) ->
    Request  = wpart:fget(?KEY_REQUEST),
    Commands = jsonutils:get_attribute(Request, ?JSON_COMMANDS),
    case length(Commands) of
        0 ->
            console:log(["Commands not found"], 'WARN');
        _ ->
            console:log(["Commands found:", Commands], 'DEBUG')
    end,
    Results  = process_commands(Controller, Commands),
    Response = {struct, [{?JSON_COMMANDS, Results}]},
    wpart:fset(?KEY_RESPONSE, Response).

%%
process_commands(Controller, Commands) ->
    process_commands(Controller, Commands, []).

process_commands(_, [], Results) ->
    lists:reverse(Results);
process_commands(Controller, [Command | Commands], Results) ->
    wpart:fset(?KEY_COMMAND_JSON, Command),
    {Type, Result} = case list_to_atom(jsonutils:get_attribute(Command, ?JSON_TYPE)) of
        undefined ->
            utils_error:insert(?ERROR_COMMAND_UNDEFINED),
            {null, try_action(Controller, default, [error])};
        Action ->
            {utils:to_binary(Action), try_action(Controller, Action, [])}
    end,
    wpart:fdelete(?KEY_COMMAND),
    process_commands(Controller, Commands, [{struct, [{?JSON_TYPE, Type} | Result]} | Results]).

%%
try_action(Controller, Action, Arguments) ->
    case (catch apply(Controller, Action, Arguments)) of
        {'EXIT', Status} ->
            console:log(["Action failed:", {Status, Controller, Action, Arguments}], 'WARN'),
            case Status of
                {undef, _Stack} ->
                    utils_error:insert(?ERROR_COMMAND_UNKNOWN),
                    try_action(Controller, default, [error]);
                _ActionFailed ->
                    try_action(Controller, default, Arguments)
            end;
        Result ->
            Result
    end.
