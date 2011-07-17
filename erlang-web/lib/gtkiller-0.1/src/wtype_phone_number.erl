-module(wtype_phone_number).

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
    format/1,
    
    freez_number/2,
    free_number/1
]).

-include("phone_number.hrl").
-include("account.hrl").

get_record_info(phone_number) -> record_info(fields, phone_number);
get_record_info(phone_number_types) -> #phone_number_types{}.

create(Arg) ->

    % new autoincremented id
    Id = db:get_next_id_if(phone_number, Arg),

    OrgId = case Arg#phone_number.account_id of
        D when is_integer(D), D > 0 -> #account{organization_id=O} = wtype_account:read(Arg#phone_number.account_id),O;
        _                           -> Arg#phone_number.organization_id
    end,

    % default fields
    PN = Arg#phone_number{
        id = Id,
        organization_id = OrgId,
        created = calendar:universal_time()
    },

    console:log(["Creating phone_number:", PN]),

    % add to accounts table
    e_db:write(phone_number, PN),

    PN.


read(all) ->
    e_db:read(phone_number);
read(Id) ->
    e_db:read(phone_number, Id).

update(Number) ->

    console:log(["phone_number UPDATE:", Number]),

    OrgId = case Number#phone_number.account_id of
        D when is_integer(D), D > 0 -> #account{organization_id=O} = wtype_account:read(Number#phone_number.account_id),O;
        _                           -> Number#phone_number.organization_id
    end,
    Number1 = Number#phone_number{
        organization_id = OrgId
    },

    e_db:update(phone_number, Number1),

    Number1.

delete(Id) ->
    e_db:delete(phone_number, Id).



prepare_initial() ->
    wpart_db:build_record_structure(phone_number, #phone_number{}).

prepare_validated() ->
    Item = wpart:fget("__not_validated"),
    wpart_db:build_record_structure(phone_number, Item).

prepare_edit(Item) ->
    wpart_db:build_record_structure(phone_number, Item).

format(Item) ->
    [{"id", Item#phone_number.id},{"number", Item#phone_number.number}].
    
freez_number(Number, AccountID) ->
    N = utils:to_list(Number),
    case db:select(phone_number, fun(E) -> element(5,E) =:= N end) of
        [R] -> update(R#phone_number{account_id=AccountID});
        _   -> ok
    end.

free_number(Number) ->
    N = utils:to_list(Number),
    case db:select(phone_number, fun(E) -> element(5,E) =:= N end) of
        [R] -> update(R#phone_number{account_id=0});
        _   -> ok
    end.