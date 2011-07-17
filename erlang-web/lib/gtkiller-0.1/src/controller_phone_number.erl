-module(controller_phone_number).

-extends(controller_crud).

-export([
    create/0,
    update/0,
    delete/0,
    select/0

]).

-include("phone_number.hrl").
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
update() -> ?BASE_MODULE:update().

%%
%%
?AUTHORIZE(json).
?CHECK_EXISTENCE({}).
delete() -> ?BASE_MODULE:delete().

?AUTHORIZE(json).
select() ->
    console:log(["Action executed:", {?MODULE, select}], 'DEBUG'),
    Command = wpart:fget(?KEY_COMMAND_JSON),
    RawFields = jsonutils:get_attribute(Command, ?TO_JSON_NAME(fields)),
    Fields = case RawFields of
        undefined -> {exclude, []};
        Value     -> {fields, Value}
    end,

    Rows = ?BASE_MODULE:get_rows(),
    FormattedRows = utils:format(phone_number, Rows, Fields),

    ?BASE_MODULE:process_selected_rows(FormattedRows).
    
    