%% The contents of this file are subject to the Erlang Web Public License,
%% Version 1.0, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Web Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang-consulting.com/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Erlang Training & Consulting
%% Ltd. Portions created by Erlang Training & Consulting Ltd are Copyright 2008,
%% Erlang Training & Consulting Ltd. All Rights Reserved.

%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wtype_enum).
-behaviour(wtype).

-export([handle_call/2, validate/1]).

-include_lib("xmerl/include/xmerl.hrl").

handle_call(_Format, Val) -> 
    Val.

validate({Types,undefined}) -> 
    case wpart_valid:is_private(Types) of
	true ->
	    {ok, undefined};
        false ->
            case lists:keysearch(optional, 1, Types) of
	       {value, {optional, Default}} -> 
                    {ok, Default};
	       _ ->  
                    {error, {empty_input, undefined}}
            end
    end;

validate({Types, Input}) ->
    case wpart_valid:is_private(Types) of
	true ->
	    {ok, Input};
	false ->
            if 
				Input =/= undefined ->  
		    		check_choices(Types, Input);
               true -> 
		    		{error, {empty_input, Input}}
            end
    end;

validate(Input) -> {error, {it_is_not_enum_value, Input}}.
	    
check_choices(Types, Input) ->
    case lists:keysearch(choices, 1, Types) of
	{_, {_, Choices}} ->
	    case lists:member(Input, prepare_choices(Choices)) of
		true ->
		    {ok, Input};
		false ->
		    {error, {no_such_enum_value, Input}}
	    end;
	_ ->
	    {ok, Input}
    end.

prepare_choices(String) ->
    lists:map(fun prepare_choices1/1, string:tokens(String, [$|])).
    
prepare_choices1(String) ->
    hd(string:tokens(String, [$:])).
