-module(wtype_acd_rules).

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
-include("acd_rules.hrl").

get_record_info(acd_rules) -> record_info(fields, acd_rules);
get_record_info(acd_rules_types) -> #acd_rules_types{}.

create(Arg) ->

    Acd = Arg#acd_rules{
        id = db:get_next_id_if(acd_rules, Arg)
    },
    
    console:log(["Creating acd:", Acd]),

    % add to accounts table
    e_db:write(acd_rules, Acd),

    Acd.


read(all) ->
    e_db:read(acd_rules);
read(Id) ->
    e_db:read(acd_rules, Id).

update(Acd) ->
    e_db:update(acd_rules, Acd),

    Acd.

delete(Id) ->
    e_db:delete(acd_rules, Id).



prepare_initial() ->
    wpart_db:build_record_structure(acd_rules, #acd_rules{}).

prepare_validated() ->
    Item = wpart:fget("__not_validated"),
    wpart_db:build_record_structure(acd_rules, Item).

prepare_edit(Item) ->
    wpart_db:build_record_structure(acd_rules, Item).

format(Item) ->
    [{"id", Item#acd_rules.id},{"name", Item#acd_rules.name}].