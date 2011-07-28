-module(controller_account_acd).

-export([
    handle/1
]).

-include("account.hrl").
-include("acd.hrl").
-include("phone_number.hrl").
-include("constants.hrl").
-include("utils_controller_annotations.hrl").

%%%
%%% Controller JSON-actions.
%%%

?PROCESS_RESPONSE({}).  
?AUTHORIZE(json).    
handle(_Args) ->
    RequestJSON = jsonutils:decode(wpart:fget("post:request")),
    [Command|_] = jsonutils:get_attribute(RequestJSON, ?JSON_COMMANDS),
    Response = {struct, [{?JSON_COMMANDS, action(jsonutils:get_attribute(Command, <<"type">>))}]},
    wpart:fset(?KEY_RESPONSE, Response).

% NEWACDs
%[{struct,[{<<"acd_id">>,1},
%                        {<<"account_id">>,6},
%                        {<<"number">>,<<"323232">>}]},
%               {struct,[{<<"acd_id">>,1},
%                        {<<"account_id">>,6},
%                        {<<"number">>,<<"45454">>}]},
%               {struct,[{<<"acd_id">>,1},
%                        {<<"account_id">>,6},
%                        {<<"number">>,<<"12121212">>}]},
%               {struct,[{<<"acd_id">>,2},
%                        {<<"account_id">>,6},
%                        {<<"number">>,<<"77777777">>}]},
%               {struct,[{<<"acd_id">>,2},
%                        {<<"account_id">>,6},
%                        {<<"number">>,<<"55555555">>}]}]
% OLDACDS
%[#acd{id = 11,acd_id = 1,account_id = 6,number = 323232,
%      active = "Y"},
% #acd{id = 9,acd_id = 1,account_id = 6,number = 45454,
%      active = "Y"},
% #acd{id = 10,acd_id = 1,account_id = 6,number = 12121212,
%      active = "Y"},
% #acd{id = 13,acd_id = 2,account_id = 6,number = 77777777,
%      active = "Y"},
% #acd{id = 12,acd_id = 2,account_id = 6,number = 55555555,
%      active = "Y"}]

action("save")    ->
    RequestJSON = jsonutils:decode(wpart:fget("post:request")),
    [Command|_] = jsonutils:get_attribute(RequestJSON, ?JSON_COMMANDS),
    AccountID   = jsonutils:get_attribute(Command, <<"account_id">>),
    
    CurrentAcds = db:select(acd, fun(#acd{account_id=AID})-> AID =:= AccountID end),
    
    NewAcds = [{acd, 0,
                element(2,lists:keyfind(<<"acd_id">>, 1, R)), AccountID,
                list_to_integer(binary_to_list(element(2,lists:keyfind(<<"number">>, 1, R)))), "Y"}
        || {struct, R} <- jsonutils:get_attribute(Command, <<"acds">>)],
    console:log(["SAVE ACDS: ", NewAcds]),    
    
    % delete records
    lists:foreach(fun(DR) ->
            console:log(["Record for delete ", DR]),
            wtype_acd:delete(element(2,DR)),
            wtype_phone_number:free_number(element(5,DR))
        end,
        lists:filter(fun(R) ->
            case
               [X || X <- NewAcds, element(3,X) =:= element(3,R),
                                   element(4,X) =:= element(4,R),
                                   element(5,X) =:= element(5,R)] of
               [] -> true;
               _  -> false
            end
        end, CurrentAcds)),
    % update records -- not needed yet
    % insert records
    lists:foreach(fun(NR) ->
            console:log(["Record for insert ", NR]),
            wtype_acd:create(NR),
            wtype_phone_number:freez_number(element(5,NR), AccountID)
        end,
        lists:filter(fun(R) ->
            case
               [X || X <- CurrentAcds,  element(3,X) =:= element(3,R),
                                        element(4,X) =:= element(4,R),
                                        element(5,X) =:= element(5,R)] of
               [] -> true;
               _  -> false
            end
        end, NewAcds)),
    
    {struct,[
	{?JSON_TYPE, <<"save">>}]};   
action("select")  ->
    RequestJSON = jsonutils:decode(wpart:fget("post:request")),
    [Command|_] = jsonutils:get_attribute(RequestJSON, ?JSON_COMMANDS),
    AccountID   = jsonutils:get_attribute(Command, <<"account_id">>),
%
% data = [{acd_id:1, numbers:[111,1112,334,443]}, {acd_id:12, numbers:[]}]
% [{acd, id, acd_id, account_id, number, active},...,{acd, id, acd_id, account_id, number, active}]
%
    Fn = fun(E, Acc) ->
        case lists:keyfind(element(3,E), 1, Acc) of
            false      -> Acc ++ [{element(3,E), [element(5,E)]}];
            {AcdID, N} -> lists:keyreplace(element(3,E), 1, Acc, {AcdID, N++[element(5,E)]})
        end
    end,
    Ret = lists:foldl(Fn, [],
        db:select(acd, fun(#acd{account_id=AID})-> AID =:= AccountID end)),
    Data = [{struct,[
        {<<"acd_id">>,  TAcd},
        {<<"numbers">>, TNum} ]}|| {TAcd, TNum} <- Ret],

    {struct,[
	{?JSON_TYPE, <<"select">>},
	{?JSON_DATA, Data}]}.
