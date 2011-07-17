-module(wtype_reseller).

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

-include("reseller.hrl").

get_record_info(reseller) -> record_info(fields, reseller);
get_record_info(reseller_types) -> #reseller_types{}.

create(Arg) ->

    % new autoincremented id
    Id = db:get_next_id_if(reseller, Arg),

    % default fields
    Reseller = Arg#reseller{
        id = Id,
        created = calendar:universal_time()
    },

    console:log(["Creating reseller:", Reseller]),

    % add to accounts table
    e_db:write(reseller, Reseller),

    Reseller.


read(all) ->
    e_db:read(reseller);
read(Id) ->
    e_db:read(reseller, Id).

update(Reseller) ->

    console:log(["reseller UPDATE:", Reseller]),

    e_db:update(reseller, Reseller),

    Reseller.

delete(Id) ->
    e_db:delete(reseller, Id).



prepare_initial() ->
    wpart_db:build_record_structure(reseller, #reseller{}).

prepare_validated() ->
    Item = wpart:fget("__not_validated"),
    wpart_db:build_record_structure(reseller, Item).

prepare_edit(Item) ->
    wpart_db:build_record_structure(reseller, Item).

format(Item) ->
    [{"id", Item#reseller.id},{"name", Item#reseller.name}].