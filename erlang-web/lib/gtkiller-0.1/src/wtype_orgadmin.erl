-module(wtype_orgadmin).

-export([
    get_record_info/1,

    % CRUD
    create/1,
    read/1,
    update/1,
    delete/1,

    prepare_initial/0,
    prepare_validated/0,
    prepare_edit/1
]).

-include("orgadmin.hrl").
-include("organization.hrl").

get_record_info(orgadmin) -> record_info(fields, orgadmin);
get_record_info(orgadmin_types) -> #orgadmin_types{}.

create(Arg) ->

    % new autoincremented id
    Id = db:get_next_id_if(orgadmin, Arg),

    #organization{reseller_id=ResellerId} = wtype_organization:read(Arg#orgadmin.organization_id),

    % default fields
    Org = Arg#orgadmin{
        id = Id,
        reseller_id = ResellerId,
        created = calendar:universal_time()
    },

    console:log(["Creating orgadmin:", Org]),

    % add to accounts table
    e_db:write(orgadmin, Org),

    Org.


read(all) ->
    e_db:read(orgadmin);
read(Id) ->
    e_db:read(orgadmin, Id).

update(Org) ->

    console:log(["orgadmin UPDATE:", Org]),

    #organization{reseller_id=ResellerId} = wtype_organization:read(Org#orgadmin.organization_id),
    Org1 = Org#orgadmin{reseller_id = ResellerId},
    e_db:update(orgadmin, Org1),
    Org1.

delete(Id) ->
    e_db:delete(orgadmin, Id).



prepare_initial() ->
    wpart_db:build_record_structure(orgadmin, #orgadmin{}).

prepare_validated() ->
    Org = wpart:fget("__not_validated"),
    wpart_db:build_record_structure(orgadmin, Org).

prepare_edit(Item) ->
    wpart_db:build_record_structure(orgadmin, Item).

