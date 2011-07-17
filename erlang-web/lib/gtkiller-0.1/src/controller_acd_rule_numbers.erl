-module(controller_acd_rule_numbers).

-extends(controller_crud).

-export([
    create/0,
    update/0,
    delete/0,
    select/0
]).

-include("account.hrl").
-include("acd.hrl").
-include("acd_rules.hrl").
-include("acd_rule_numbers.hrl").
-include("constants.hrl").
-include("utils_controller_annotations.hrl").

%%%
%%% Controller JSON-actions.
%%%

%%
%%
?PREPARE_DATA({}).
?VALIDATE_DATA({}).
create() -> ?BASE_MODULE:create().

%%
%%
?AUTHORIZE(json).
?CHECK_EXISTENCE({}).
?PREPARE_DATA({}).
update() ->
    console:log(["Action executed MT1:", {?MODULE, update}], 'DEBUG'),
    console:log(["update, BASE_MODULE:", ?BASE_MODULE], 'DEBUG'),
    ?BASE_MODULE:update().

%%
%%
?AUTHORIZE(json).
?CHECK_EXISTENCE({}).
delete() -> ?BASE_MODULE:delete().

?AUTHORIZE(json).
select() ->
    console:log(["LALALA Action executed:", {?MODULE, select}], 'DEBUG'),
    Command = wpart:fget(?KEY_COMMAND_JSON),
    RawFields = jsonutils:get_attribute(Command, ?TO_JSON_NAME(fields)),
    console:log(["CAARN1: ", Command, RawFields], 'DEBUG'),
    Fields = case RawFields of
        undefined -> {exclude, []};
        Value     -> {fields, Value}
    end,
    console:log(["CAARN2: ", Fields], 'DEBUG'),

    Rows = ?BASE_MODULE:get_rows(),
    console:log(["CAARN3: ", Rows], 'DEBUG'),
    FormattedRows = utils:format(acd_rule_numbers, Rows, Fields),
    console:log(["CAARN4: ", FormattedRows], 'DEBUG'),

    ?BASE_MODULE:process_selected_rows(FormattedRows).
