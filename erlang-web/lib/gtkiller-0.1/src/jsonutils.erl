-module(jsonutils).

-export([
    encode/1,
    decode/1,

    get_attribute/2,
    get_mochi_attribute/2,

    test_mochi/0,

    to_value/1
]).

encode(MochiJSON) ->
    Encode = mochijson2:encoder([{utf8, true}]),
    Encode(MochiJSON).

decode(JSON) ->
    mochijson2:decode(JSON).

%%
%% converts binary value to list,
%% leaves other the same
%%
to_value(X) when is_binary(X) -> binary_to_list(X);
to_value(X) -> X.

get_attribute(JSON, Attr) ->
    case get_mochi_attribute(JSON, Attr) of
	    List when is_list(List) -> lists:map(fun jsonutils:to_value/1, List);
	    Other -> to_value(Other)
    end.

get_mochi_attribute({struct, PropList}, Attr) when is_list(PropList) ->
    proplists:get_value(Attr, PropList).

test_mochi() ->
    D = {struct,
      [{<<"type">>,<<"select">>},
       {<<"needFields">>,true},
       {<<"needRules">>,true},
       {<<"needTotalSize">>,true},
       {<<"filters">>,[]},
       {<<"orderBy">>,
        [{struct,[{<<"field">>,<<"owner_last_name">>}]},
         {struct,[{<<"field">>,<<"owner_first_name">>}]}]},
       {<<"page">>,
        {struct,
            [{<<"index">>,1},
             {<<"size">>,100},
             {<<"where">>,null}]}},
       {<<"source">>,<<"users">>}]},
    V = get_mochi_attribute(D, <<"source">>),
    V.

