-module(wtype_acd_rule_numbers).

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

-include("acd_rule_numbers.hrl").
-include("acd_rules.hrl").

get_record_info(acd_rule_numbers) -> record_info(fields, acd_rule_numbers);
get_record_info(acd_rule_numbers_types) -> #acd_rule_numbers_types{}.

create(Arg) ->

    Acd = Arg#acd_rule_numbers{
        id = db:get_next_id_if(acd_rule_numbers, Arg)
    },
    
    console:log(["Creating acd_rule_numbers:", Acd]),

    % add to accounts table
    e_db:write(acd_rule_numbers, Acd),

    Acd.


read(all) ->
    e_db:read(acd_rule_numbers);
read(Id) ->
    e_db:read(acd_rule_numbers, Id).

update(Acd) ->
    e_db:update(acd_rule_numbers, Acd),

    Acd.

delete(Id) ->
    e_db:delete(acd_rule_numbers, Id).



prepare_initial() ->
    wpart_db:build_record_structure(acd_rule_numbers, #acd_rule_numbers{}).

prepare_validated() ->
    Item = wpart:fget("__not_validated"),
    wpart_db:build_record_structure(acd_rule_numbers, Item).

prepare_edit(Item) ->
    wpart_db:build_record_structure(acd_rule_numbers, Item).

format(Item) ->
    [{"id", Item#acd_rule_numbers.id},{"name", Item#acd_rule_numbers.number}].