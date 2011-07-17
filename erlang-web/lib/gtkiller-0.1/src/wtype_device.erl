-module(wtype_device).

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

-include("device.hrl").
-include("account.hrl").

get_record_info(device) -> record_info(fields, device);
get_record_info(device_types) -> #device_types{}.

create(Arg) ->

    % new autoincremented id
    Id = db:get_next_id_if(device, Arg),

    OrgId = case Arg#device.account_id of
        D when is_integer(D), D > 0 -> #account{organization_id=O} = wtype_account:read(Arg#device.account_id),O;
        _                           -> Arg#device.organization_id
    end,

    % default fields
    Reseller = Arg#device{
        id = Id,
        organization_id = OrgId,
        created = calendar:universal_time()
    },

    console:log(["Creating reseller:", Reseller]),

    % add to accounts table
    e_db:write(device, Reseller),

    Reseller.


read(all) ->
    e_db:read(device);
read(Id) ->
    e_db:read(device, Id).

update(Device) ->

    console:log(["device UPDATE:", Device]),

    OrgId = case Device#device.account_id of
        D when is_integer(D), D > 0 -> #account{organization_id=O} = wtype_account:read(Device#device.account_id),O;
        _                           -> Device#device.organization_id
    end,
    Device1 = Device#device{
        organization_id = OrgId
    },

    e_db:update(device, Device1),

    Device1.

delete(Id) ->
    e_db:delete(device, Id).



prepare_initial() ->
    wpart_db:build_record_structure(device, #device{}).

prepare_validated() ->
    Item = wpart:fget("__not_validated"),
    wpart_db:build_record_structure(device, Item).

prepare_edit(Item) ->
    wpart_db:build_record_structure(device, Item).

format(Item) ->
    [{"id", Item#device.id},{"name", Item#device.name}].