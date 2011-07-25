-module(controller_acd_rule_numbers).

-export([
    handle/1,
    get/1,
    update/1
]).

-include("account.hrl").
-include("acd.hrl").
-include("acd_rules.hrl").
-include("acd_rule_numbers.hrl").
-include("constants.hrl").
-include("utils_controller_annotations.hrl").

?PROCESS_RESPONSE({}).  
?AUTHORIZE(json).    
handle(_Args) ->
    RequestJSON = jsonutils:decode(wpart:fget("post:request")),
    [Command|_] = jsonutils:get_attribute(RequestJSON, ?JSON_COMMANDS),
    Response = {struct, [{?JSON_COMMANDS, action(jsonutils:get_attribute(Command, <<"type">>))}]},
    wpart:fset(?KEY_RESPONSE, Response).
    
action(A) ->
    {struct,[
	{?JSON_TYPE, utils:to_binary(A)},
	{<<"error">>, <<"unexpected action">>}]}.

?AUTHORIZE(json).    
get(_Args) ->
    console:log(["ACD RULE NUMBERS GET"]),
    AcdtId = utils:to_list(proplists:get_value(acdid, _Args)),
    AcdtRd = utils:to_list(proplists:get_value(ruleid, _Args)),
    Sord   = utils:to_list(wpart:fget("get:sord")), % asc|desc
    Sidx   = utils:to_list(wpart:fget("get:sidx")),  
    console:log(["ACD RULE NUMBERS GET: ", AcdtId, AcdtRd, Sord, Sidx]),
    
    Idx = case Sidx of
        "order"  -> 4;
        _        -> 2
    end,
    Order = case Sord of
        "asc" -> ascending;
        _     -> descending
    end,
    
    console:log(["ACD RULE NUMBERS ORDER: ", Idx, Order]),
    
    %Records = db:select(acd_rule_numbers, fun(#acd_rule_numbers{acd_rule_id=ACDRID}) when ACDRID=:=AcdtRd->true;(_)->false end),
    Records = db:select(acd_rule_numbers, [
        {where, fun(#acd_rule_numbers{acd_rule_id=ACDRID}) when ACDRID=:=AcdtRd->true;(_)->false end},
        {order, {Idx, Order}}]),
    
    console:log(["ACD RULE NUMBERS GET: Records: ", Records]),

    FR = {struct,[
        {<<"page">>,1},
        {<<"total">>,1},
        {<<"records">>,length(Records)},
        {<<"rows">>, [ {struct, [{<<"id">>,ID},
            {<<"cell">>, [ID,
                utils:to_binary(O), utils:to_binary(FT),
                utils:to_binary(N), utils:to_binary(TO), utils:to_binary(A)]}]}
            || {_, ID, _, O, FT, N, TO, A} <- Records]}
    ]},

    {content, text, mochijson2:encode(FR)}.

?AUTHORIZE(json).    
update(_Args) ->
    console:log(["ACD RULE NUMBERDS ADD/DEL/EDIT"]),
    %AcdtId      = proplists:get_value(acdid, _Args),
    AcdtRd      = proplists:get_value(ruleid, _Args),
    
    ID          = wpart:fget("post:id"),
    Oper        = wpart:fget("post:oper"),
    Active      = wpart:fget("post:active"),
    Order       = wpart:fget("post:order"),
    ForwardType = wpart:fget("post:forward_type"),
    Number      = wpart:fget("post:number"),
    Timeout     = wpart:fget("post:timeout"),
    
    console:log(["ACD RULE NUMBERDS ADD/DEL/EDIT OPER: ", Oper, utils:to_integer(ID)]),
    
    _ACDR = case Oper of
        "edit"   ->
            wtype_acd_rule_numbers:update(#acd_rule_numbers{
                id          = utils:to_integer(ID),
                acd_rule_id      = AcdtRd,
                order            = Order,
                forward_type     = ForwardType,
                number           = Number,
                timeout          = Timeout,
                active           = Active
            });    
        "del"    -> wtype_acd_rule_numbers:delete(utils:to_integer(ID));
        "add"    ->
            wtype_acd_rule_numbers:create(#acd_rule_numbers{
                acd_rule_id      = AcdtRd,
                order            = Order,
                forward_type     = ForwardType,
                number           = Number,
                timeout          = Timeout,
                active           = Active
            });
        _ -> console:log(["UNKNOWN OPERATION:", Oper]), ok
    end,
    
    Records = db:select(acd_rule_numbers, fun(#acd_rule_numbers{acd_rule_id=ACDRID}) when ACDRID=:=AcdtRd->true;(_)->false end),
    
    console:log(["ACD RULE NUMBERS GET: Records: ", Records]),

    FR = {struct,[
        {<<"page">>,1},
        {<<"total">>,1},
        {<<"records">>,length(Records)},
        {<<"rows">>, [ {struct, [{<<"id">>,ID},
            {<<"cell">>, [ID,
                utils:to_binary(O), utils:to_binary(FT),
                utils:to_binary(N), utils:to_binary(TO), utils:to_binary(A)]}]}
            || {_, ID, _, O, FT, N, TO, A} <- Records]}
    ]},

    {content, text, mochijson2:encode(FR)}.