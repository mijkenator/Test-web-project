-module(wtype_device_line).

-export([
    get_record_info/1,

    % CRUD
    create/1,
    read/1,
    update/1,
    delete/1,

    prepare_initial/0,
    prepare_validated/0,
    prepare_edit/1,
    format/1
]).

-include("device_line.hrl").
-include("device.hrl").

get_record_info(device_line) -> record_info(fields, device_line);
get_record_info(device_line_types) -> #device_line_types{}.

create(Arg) ->

    % new autoincremented id
    Id = db:get_next_id_if(device_line, Arg),

    % default fields
    Reseller = Arg#device_line{
        id = Id
    },

    console:log(["Creating device line:", Reseller]),

    % add to accounts table
    e_db:write(device_line, Reseller),

    Reseller.


read(all) ->
    e_db:read(device_line);
read(Id) ->
    e_db:read(device_line, Id).

update(DeviceLine) ->

    console:log(["device line UPDATE:", DeviceLine]),

    e_db:update(device, DeviceLine),

    DeviceLine.

delete(Id) ->
    e_db:delete(device_line, Id).



prepare_initial() ->
    wpart_db:build_record_structure(device_line, #device_line{}).

prepare_validated() ->
    Item = wpart:fget("__not_validated"),
    wpart_db:build_record_structure(device_line, Item).

prepare_edit(Item) ->
    wpart_db:build_record_structure(device_line, Item).

format(Item) ->
    [{"id", Item#device_line.id},{"name", Item#device_line.name}].