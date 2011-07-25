-module(controller_acd_rules).

-extends(controller_crud).

-export([
    handle/1,
    get/1,
    add/1,
    delete/1,
    update/1
]).

-include("account.hrl").
-include("acd.hrl").
-include("acd_rules.hrl").
-include("phone_number.hrl").
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
    console:log(["ACD RULES GET"]),
    AcdtId = utils:to_list(proplists:get_value(id, _Args)),
    console:log(["ACD RULES GET: ", AcdtId]),
    
    Sord   = utils:to_list(wpart:fget("get:sord")), % asc|desc
    Sidx   = utils:to_list(wpart:fget("get:sidx")),  
    console:log(["ACD RULES GET: ", AcdtId, Sord, Sidx]),
    
    Idx = case Sidx of
        "priority"  -> 6;
        _           -> 2
    end,
    Order = case Sord of
        "asc" -> ascending;
        _     -> descending
    end,
    
    %Records = db:select(acd_rules, fun(#acd_rules{acd_id=ACDID}) when ACDID=:=AcdtId->true;(_)->false end),
    Records = db:select(acd_rules, [
        {where, fun(#acd_rules{acd_id=ACDID}) when ACDID=:=AcdtId->true;(_)->false end},
        {order, {Idx, Order}}]),
    
    console:log(["ACD RULES GET: Records: ", Records]),

    FR = {struct,[
        {<<"page">>,1},
        {<<"total">>,1},
        {<<"records">>,length(Records)},
        {<<"rows">>, [ {struct, [{<<"id">>,ID},
            {<<"cell">>, [ID, utils:to_binary(P), utils:to_binary(Name), utils:to_binary(TP), utils:to_binary(A)]}]}
            || {_, ID, _, Name, TP, P, A} <- Records]}
    ]},

    {content, text, mochijson2:encode(FR)}.
    
?AUTHORIZE(json).    
add(_Args) ->
    console:log(["ACD RULES ADD"]),
    AcdtId      = proplists:get_value(id, _Args),
    Active      = wpart:fget("post:active"),
    Name        = wpart:fget("post:name"),
    Priority    = wpart:fget("post:priority"),
    TimePeriod  = wpart:fget("post:time_period"),
    
    ACDR = wtype_acd_rules:create(#acd_rules{
        acd_id      = AcdtId,
        name        = Name,
        time_period = TimePeriod,
        priority    = Priority,
        active      = Active
    }),
    
    console:log(["NEW ACDR: ", ACDR]),
    
    Records = db:select(acd_rules, fun(#acd_rules{acd_id=ACDID}) when ACDID=:=AcdtId->true;(_)->false end),
    
    console:log(["ACD RULES GET: Records: ", Records]),

    FR = {struct,[
        {<<"page">>,1},
        {<<"total">>,1},
        {<<"records">>,length(Records)},
        {<<"rows">>, [ {struct, [{<<"id">>,ID},
            {<<"cell">>, [ID, utils:to_binary(P), utils:to_binary(Name), utils:to_binary(TP), utils:to_binary(A)]}]}
            || {_, ID, _, Name, TP, P, A} <- Records]}
    ]},

    {content, text, mochijson2:encode(FR)}.

?AUTHORIZE(json).    
delete(_Args) ->
    console:log(["ACD RULES ADD"]),
    AcdtId      = proplists:get_value(id, _Args),
    ID          = wpart:fget("post:id"),
    
    wtype_acd_rules:delete(utils:to_integer(ID)),
    
    Records = db:select(acd_rules, fun(#acd_rules{acd_id=ACDID}) when ACDID=:=AcdtId->true;(_)->false end),
    
    console:log(["ACD RULES GET: Records: ", Records]),

    FR = {struct,[
        {<<"page">>,1},
        {<<"total">>,1},
        {<<"records">>,length(Records)},
        {<<"rows">>, [ {struct, [{<<"id">>,ID},
            {<<"cell">>, [ID, utils:to_binary(P), utils:to_binary(Name), utils:to_binary(TP), utils:to_binary(A)]}]}
            || {_, ID, _, Name, TP, P, A} <- Records]}
    ]},

    {content, text, mochijson2:encode(FR)}.


?AUTHORIZE(json).    
update(_Args) ->
    console:log(["ACD RULES ADD"]),
    AcdtId      = proplists:get_value(id, _Args),
    ID          = wpart:fget("post:id"),
    Active      = wpart:fget("post:active"),
    Name        = wpart:fget("post:name"),
    Priority    = wpart:fget("post:priority"),
    TimePeriod  = wpart:fget("post:time_period"),
    
    ACDR = wtype_acd_rules:update(#acd_rules{
        id          = utils:to_integer(ID),
        acd_id      = AcdtId,
        name        = Name,
        time_period = TimePeriod,
        priority    = Priority,
        active      = Active
    }),
    
    Records = db:select(acd_rules, fun(#acd_rules{acd_id=ACDID}) when ACDID=:=AcdtId->true;(_)->false end),
    
    console:log(["ACD RULES GET: Records: ", Records]),

    FR = {struct,[
        {<<"page">>,1},
        {<<"total">>,1},
        {<<"records">>,length(Records)},
        {<<"rows">>, [ {struct, [{<<"id">>,ID},
            {<<"cell">>, [ID, utils:to_binary(P), utils:to_binary(Name), utils:to_binary(TP), utils:to_binary(A)]}]}
            || {_, ID, _, Name, TP, P, A} <- Records]}
    ]},

    {content, text, mochijson2:encode(FR)}.