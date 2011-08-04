-module(controller_device).

-extends(controller_crud).

-export([
    create/0,
    update/0,
    delete/0,
    select/0,
    get_account_lines/0
]).

-include("device.hrl").
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
    console:log(["Action executed:", {?MODULE, select}], 'DEBUG'),
    Command = wpart:fget(?KEY_COMMAND_JSON),
    RawFields = jsonutils:get_attribute(Command, ?TO_JSON_NAME(fields)),
    Fields = case RawFields of
        undefined -> {exclude, []};
        Value     -> {fields, Value}
    end,

    Rows = ?BASE_MODULE:get_rows(),
    FormattedRows = utils:format(device, Rows, Fields),

    ?BASE_MODULE:process_selected_rows(FormattedRows).
    
?AUTHORIZE(json).
get_account_lines() ->
    console:log(["GAL Action executed:", {?MODULE, get_account_lines}], 'DEBUG'),
    Command = wpart:fget(?KEY_COMMAND_JSON),
    AccountID   = jsonutils:get_attribute(Command, <<"account_id">>),
 
    [{<<"data">>, [ [DevName, LineId] || {DevName, LineId} <- wtype_device_line:get_account_all_lines(AccountID)]}].


