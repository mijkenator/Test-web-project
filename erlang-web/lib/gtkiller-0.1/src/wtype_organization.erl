-module(wtype_organization).

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

-include("organization.hrl").

get_record_info(organization) -> record_info(fields, organization);
get_record_info(organization_types) -> #organization_types{}.

create(OrgArg) ->

    % new autoincremented id
    Id = db:get_next_id_if(organization, OrgArg),

    % default fields
    Org = OrgArg#organization{
        id = Id,
        created = calendar:universal_time()
    },

    console:log(["Creating organization:", Org]),

    % add to accounts table
    e_db:write(organization, Org),

    Org.


read(all) ->
    e_db:read(organization);
read(Id) ->
    e_db:read(organization, Id).

update(Org) ->

    console:log(["organization UPDATE:", Org]),

    e_db:update(organization, Org),

    Org.

delete(Id) ->
    e_db:delete(organization, Id).



prepare_initial() ->
    wpart_db:build_record_structure(organization, #organization{}).

prepare_validated() ->
    Org = wpart:fget("__not_validated"),
    wpart_db:build_record_structure(organization, Org).

prepare_edit(Item) ->
    wpart_db:build_record_structure(organization, Item).

