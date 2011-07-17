-module(wtype_acd).

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

-include("acd.hrl").
-include("account.hrl").

get_record_info(acd) -> record_info(fields, acd);
get_record_info(acd_types) -> #acd_types{}.

create(Arg) ->

    Acd0 = Arg#acd{
        id = db:get_next_id_if(acd, Arg)
    },
    AcdID = case element(3,Acd0) of
        undefined -> e_db:get_next_id(acd_id);
        Id        -> Id
    end,
    Acd = Acd0#acd{
        acd_id = AcdID
    },
    
    console:log(["Creating acd:", Acd]),

    % add to accounts table
    e_db:write(acd, Acd),

    Acd.


read(all) ->
    e_db:read(acd);
read(Id) ->
    e_db:read(acd, Id).

update(Acd) ->
    e_db:update(acd, Acd),

    Acd.

delete(Id) ->
    e_db:delete(acd, Id).



prepare_initial() ->
    wpart_db:build_record_structure(acd, #acd{}).

prepare_validated() ->
    Item = wpart:fget("__not_validated"),
    wpart_db:build_record_structure(acd, Item).

prepare_edit(Item) ->
    wpart_db:build_record_structure(acd, Item).

format(Item) ->
    [{"id", Item#acd.id},{"name", Item#acd.number}].