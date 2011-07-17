-module(controller_crud).

-extends(controller_base).

-export([
    select/0, select/1,
    create/0, create/1,
    update/0, update/1,
    delete/0, delete/1,
    get_type/0,
    get_rows/0, get_rows/1,
    process_selected_rows/1, process_selected_rows/2
]).

-include("constants.hrl").
-include("utils_controller_annotations.hrl").

%%%
%%% Controller JSON-actions.
%%%

%%

?AUTHORIZE(json).
select() ->
    console:log(["Action executed:", {?MODULE, select}], 'DEBUG'),
    Command = wpart:fget(?KEY_COMMAND_JSON),

    % List of needed fields in response.
    Fields = case jsonutils:get_attribute(Command, ?TO_JSON_NAME(fields)) of
        undefined ->
            {exclude, []};
        Value ->
            {fields, Value}
    end,

    Rows = get_rows(),
    FormattedRows = utils:format(get_type(), Rows, Fields),

    process_selected_rows(FormattedRows).


select(error) ->
    ?MODULE:default(error).

get_rows() ->
    get_rows(get_type()).
%%
%% main select result
%% unformatted
%%
get_rows(Type) ->
    Command = wpart:fget(?KEY_COMMAND_JSON),

    % Main select result,
    % considering filters and orderBy
    db:select(Type, {
        jsonutils:get_attribute(Command, ?TO_JSON_NAME(filter)),
        jsonutils:get_attribute(Command, ?TO_JSON_NAME(orderBy))
    }).

process_selected_rows(FormattedRows) ->
    process_selected_rows(get_type(), FormattedRows).
%%
%% additional select result
%% formatted
%%
process_selected_rows(Type, FormattedRows) ->
    Command = wpart:fget(?KEY_COMMAND_JSON),
    TotalRows = length(FormattedRows),
    console:log(["TotalRows:", TotalRows]),

    % considering offset of page info
    Data = case jsonutils:get_attribute(Command, ?TO_JSON_NAME(page)) of
        % all data
        undefined ->
            [{?JSON_DATA, FormattedRows}];
        PageInfo ->
            console:log(["PageInfo:", PageInfo]),
            % zero based page number
            Page = utils:to_integer(jsonutils:get_attribute(PageInfo, ?TO_JSON_NAME(index))),
            console:log(["Page:", Page]),
            % rows per page
            Size = utils:to_integer(jsonutils:get_attribute(PageInfo, ?TO_JSON_NAME(size))),
            From = case (Page * Size) + 1 of
                BadOffset when BadOffset > TotalRows -> 1;
                GoodOffset -> GoodOffset
            end,
            To = (From + Size) - 1,
            SlicedRows = lists:sublist(FormattedRows, From, To),
            Ret = [{?JSON_DATA, SlicedRows}, {?TO_JSON_NAME(page), PageInfo}],
	    Ret
    end,
    % Field definitions and constraints.
    WithFields = case jsonutils:get_attribute(Command, ?TO_JSON_NAME(needFields)) of
        undefined ->
            Data;
        false ->
            Data;
        true ->
            Data ++ [{?TO_JSON_NAME(fields), db:get_fields_info(Type)}]
    end,
    % data with total size
    case jsonutils:get_attribute(Command, ?TO_JSON_NAME(needTotalSize)) of
        undefined ->
            WithFields;
        false ->
            WithFields;
        true ->
            WithFields ++ [{?TO_JSON_NAME(totalSize), TotalRows}]
    end.

%%
?PREPARE_DATA({}).
?VALIDATE_DATA({}).
create() ->
    console:log(["Action executed:", {?MODULE, create}], 'DEBUG'),
    Type  = get_type(),
    console:log(["Action executed: Type", Type], 'DEBUG'),
    Model = db:get_model_for(Type),
    console:log(["Action executed: Model", Model], 'DEBUG'),
    Item  = Model:create(wpart:fget(?KEY_COMMAND_DATA)),
    [{?JSON_DATA, utils:format(Type, Item)}].

create(error) ->
    ?MODULE:default(error).

%%
?AUTHORIZE(json).
?CHECK_EXISTENCE({}).
?PREPARE_DATA({}).
?VALIDATE_DATA({}).
update() ->
    console:log(["Action executed:", {?MODULE, update}], 'DEBUG'),
    Type  = get_type(),
    Model = db:get_model_for(Type),
    Item  = Model:update(wpart:fget(?KEY_COMMAND_DATA)),
    Id    = element(2, Item),
    [{?JSON_ID, Id}, {?JSON_DATA, utils:format(Type, Item, {exclude, [id]})}].

update(error) ->
    ?MODULE:default(error).

%%
?AUTHORIZE(json).
?CHECK_EXISTENCE({}).
delete() ->
    console:log(["Action executed:", {?MODULE, delete}], 'DEBUG'),
    Type  = get_type(),
    Model = db:get_model_for(Type),
    Id    = wpart:fget(?KEY_COMMAND_ID),
    Model:delete(Id),
    console:log(["Item deleted:", Id], 'DEBUG'),
    [{?JSON_ID, Id}].

delete(error) ->
    ?MODULE:default(error).

%%%
%%% Utils.
%%%

%%
% TODO: Ugly hack. Refactor this.
get_type()->
    list_to_existing_atom(lists:nthtail(11, atom_to_list(?MODULE:get_controller()))).
