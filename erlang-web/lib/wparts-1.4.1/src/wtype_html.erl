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
%%% @author Michal Ptaszek  <info@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wtype_html).
-export([handle_call/2, parse_html/3]).
-export([htmlize/1]).

-include_lib("xmerl/include/xmerl.hrl").

handle_call(_, #xmlText{} = E) ->
    E#xmlText{type = cdata};
handle_call(_, Text) ->
    Text.

%%
%% Taken from yaws_api module
%% The whole license could be found in root directory of attached yaws
%% application.
%%
htmlize(Bin) when is_binary(Bin) ->
    list_to_binary(htmlize_l(binary_to_list(Bin)));
htmlize(List) when is_list(List) ->
    htmlize_l(List).

htmlize_l(List) ->
    htmlize_l(List, []).

htmlize_l([], Acc) -> 
    lists:reverse(Acc);
htmlize_l([$>|Tail], Acc) ->
    htmlize_l(Tail, [$;,$t,$g,$&|Acc]);
htmlize_l([$<|Tail], Acc) ->
    htmlize_l(Tail, [$;,$t,$l,$&|Acc]);
htmlize_l([$&|Tail], Acc) ->
    htmlize_l(Tail, [$;,$p,$m,$a,$&|Acc]);
htmlize_l([34|Tail], Acc) -> %% $"
    htmlize_l(Tail, [$; , $t, $o,  $u,  $q  ,$&|Acc]);
htmlize_l([X|Tail], Acc) when is_integer(X) ->
    htmlize_l(Tail, [X|Acc]);
htmlize_l([X|Tail], Acc) when is_binary(X) ->
    X2 = htmlize_l(binary_to_list(X)),
    htmlize_l(Tail, [X2|Acc]);
htmlize_l([X|Tail], Ack) when is_list(X) ->
    X2 = htmlize_l(X),
    htmlize_l(Tail, [X2|Ack]).

parse_html([$<, $/ | Rest], Whitelist, Opened) ->
    parse_html_close_tag([], Rest, Whitelist, Opened);
parse_html([$< | Rest], Whitelist, Opened) ->
    parse_html_tag([], Rest, Whitelist, Opened);
parse_html([_ | Rest], Whitelist, Opened) ->
    parse_html(Rest, Whitelist, Opened);
parse_html([], _, ["html"]) ->
    ok;
parse_html([], _, Opened) ->
    ["html" | Rest] = lists:reverse(Opened),
    {error, {tags_not_closed, lists:reverse(Rest)}}.

parse_html_tag(Body, [$> | Rest], Whitelist, Opened) ->
    Tag = string:to_lower(lists:reverse(Body)),
    case lists:member(Tag, Whitelist) of
	true ->
	    parse_html(Rest, Whitelist, [Tag | Opened]);
	false ->
	    {error, {tag_not_in_whitelist, Tag}}
    end;
parse_html_tag(Body, [$/, $> | Rest], Whitelist, Opened) ->
    Tag = string:to_lower(lists:reverse(Body)),
    case lists:member(Tag, Whitelist) of
	true ->
	    parse_html(Rest, Whitelist, Opened);
	false ->
	    {error, {tag_not_in_whitelist, Tag}}
    end;
parse_html_tag(Body, [$  | Rest], Whitelist, Opened) ->
    Tag = string:to_lower(lists:reverse(Body)),
    case lists:member(Tag, Whitelist) of
	true ->
	    parse_html_tag_attr(Tag, Rest, Whitelist, Opened);
	false ->
	    {error, {tag_not_in_whitelist, Tag}}
    end;
parse_html_tag(_, [$< | _], _, [H | _]) ->
    {error, {open_tag_inside_tag, H}};
parse_html_tag(Body, [L | Rest], Whitelist, Opened) ->
    parse_html_tag([L | Body], Rest, Whitelist, Opened);
parse_html_tag(Tag, [], _, _) ->
    {error, {no_closing_tag, Tag}}.

parse_html_tag_attr(_, [$< | _], _, [H | _]) ->
    {error, {open_tag_inside_tag, H}};
parse_html_tag_attr(Tag, [$> | Rest], Whitelist, Opened) ->
    parse_html(Rest, Whitelist, [Tag | Opened]);
parse_html_tag_attr(_, [$/, $> | Rest], Whitelist, Opened) ->
    parse_html(Rest, Whitelist, Opened);
parse_html_tag_attr(Tag, [$=, 32, 34 | Rest], Whitelist, Opened) ->
    parse_html_tag_attr_inside(Tag, Rest, Whitelist, Opened);
parse_html_tag_attr(Tag, [$=, 34 | Rest], Whitelist, Opened) ->
    parse_html_tag_attr_inside(Tag, Rest, Whitelist, Opened);
parse_html_tag_attr(Tag, [$=, _ | _], _, _) ->
    {error, {no_open_quote, Tag}};
parse_html_tag_attr(Tag, [_ | Rest], Whitelist, Opened) ->
    parse_html_tag_attr(Tag, Rest, Whitelist, Opened);
parse_html_tag_attr(Tag, [], _, _) ->
    {error, {no_closing_tag, Tag}}.

parse_html_close_tag(_, [$< | _], _, [H | _Opened]) ->
    {error, {open_tag_inside_tag, H}};
parse_html_close_tag(Body, [$> | Rest], Whitelist, [H | Opened]) ->
    Tag = string:to_lower(lists:reverse(Body)),
    if
	H == Tag ->
	    parse_html(Rest, Whitelist, Opened);
	true ->
	    {error, {closing_bad_tag, {H, Tag}}}
    end;
parse_html_close_tag(Body, [L | Rest], Whitelist, Opened) ->
    parse_html_close_tag([L | Body], Rest, Whitelist, Opened).

parse_html_tag_attr_inside(Tag, [92, 34 | Rest], Whitelist, Opened) ->
    parse_html_tag_attr_inside(Tag, Rest, Whitelist, Opened);
parse_html_tag_attr_inside(Tag, [34 | Rest], Whitelist, Opened) ->
    parse_html_tag_attr(Tag, Rest, Whitelist, Opened);
parse_html_tag_attr_inside(_, [$< | _], _, [H | _]) ->
    {error, {open_tag_inside_attr, H}};
parse_html_tag_attr_inside(Tag, [_ | Rest], Whitelist, Opened) ->
    parse_html_tag_attr_inside(Tag, Rest, Whitelist, Opened);
parse_html_tag_attr_inside(Tag, [], _, _) ->
    {error, {no_closing_tag, Tag}}.
