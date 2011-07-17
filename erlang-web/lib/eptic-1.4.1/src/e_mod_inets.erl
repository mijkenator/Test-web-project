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
%%% File	: e_mod_inets.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Inets server callback module.
%%% @end
%%%-------------------------------------------------------------------
-module(e_mod_inets).

-export([do/1]).
-export([fe_request/2]).

-export([cookie_up/1, cookie_bind/1, cleanup/0]).
-export([parse_post/1, parse_get/1, fetch_boundary/1, format_response/1]).

-include_lib("/usr/lib64/erlang/lib/inets-5.5.1/src/http_server/httpd.hrl").
-include_lib("eptic/include/eptic.hrl").

%%====================================================================
%% API
%% @hidden
%%====================================================================
do(#mod{parsed_header = Headers} = A) ->
    e_logger:register_pid(self()),
    case handle_args(A) of
	{ok, Args} ->
	    [$/ | URL] = A#mod.request_uri,
	    e_dict:init_state(Args),

	    e_logger:log({?MODULE, {url, URL}}),

	    case A#mod.socket_type of 
		{ssl, _} ->
		    e_dict:fset("__https", true),
		    e_dict:fset("__ip", get_ip(ssl:peername(A#mod.socket)));
		_ ->
		    e_dict:fset("__https", false),
		    e_dict:fset("__ip", get_ip(inet:peername(A#mod.socket)))
	    end,

	    ClientCookie = cookie_up(Headers),

	    e_dict:fset("__path", URL),
	    e_dict:fset("__cookie_key", ClientCookie),

	    ControllerFun = fun() ->
				    case e_mod_gen:handle_request(A#mod.request_uri) of
					{ret_view, Ret, View} ->
					    controller_exec(Ret, View);
					[_Status, _Html] = Error ->
					    Error;
					{html, HTML} ->
					    controller_exec({content, html, HTML}, []);
					enoent ->
					    enoent;
					Else ->
					    controller_exec(Else, "")
				    end
			    end,

	    case with_formatted_error(ControllerFun) of
		{NewHeaders, Result} ->
		    CookieHeader = cookie_bind(ClientCookie),
		    cleanup(),

		    e_logger:unregister_pid(self()),
		    {proceed, [{response, {response, [CookieHeader | NewHeaders], Result}}]};
		enoent ->
		    cookie_bind(ClientCookie), 
		    cleanup(),

		    e_logger:unregister_pid(self()),
		    {proceed, A#mod.data}
	    end
    end.

%% @hidden
-spec(fe_request/2 :: (tuple(), term()) -> {term(), term()}).
fe_request(#mod{parsed_header = Headers} = A, Session) ->
    Cookie = proplists:get_value(?COOKIE, get_cookies(Headers), []),
    e_session:update_session(Cookie, Session),
    
    Ret = do(A),
    
    {ok, NewSession} = e_session:get_session(Cookie),
    {Ret, NewSession}.

%%====================================================================
%% Internal functions
%%====================================================================
-spec(handle_args/1 :: (tuple()) -> {ok, list(tuple())}).	     
handle_args(#mod{method = Method, entity_body = Post} = Mod) ->
    Result = case Method of
		 "POST" ->
		     {ok, [{"get", parse_get(Mod#mod.request_uri)},
			   {"post", parse_post(Post)}]};
		 _ ->
		     {ok, [{"get", parse_get(Mod#mod.request_uri)}]}
	     end,
    e_logger:log({?MODULE, {handle_args, Result}}),
    Result.

-spec(controller_exec/2 :: (e_mod_gen:controller_response(), string()) -> {list(tuple()), string()}).	     
controller_exec(Ret, View) ->
    case Ret of
	template ->
	    format_response(e_mod_gen:template(e_mod_gen:template_file(View), [],
					       e_conf:template_expander()));
	{redirect, URL} ->
	    {[{code, 302}, {location, URL}, {content_length, "0"}], []};
	{content, html, Data} ->
	    Length = {content_length, integer_to_list(erlang:iolist_size(Data))},
	    {[{content_type, "text/html"}, {code, 200}, Length], Data};
	{content, xml, Data} ->
	    Length = {content_length, integer_to_list(erlang:iolist_size(Data))},
	    {[{content_type, "application/xml"}, {code, 200}, Length], Data};
	{content, text, Data} ->
	    Length = {content_length, integer_to_list(erlang:iolist_size(Data))},
	    {[{content_type, "text/plain"}, {code, 200}, Length], Data};
	{content, pdf, Data} ->
	    Length = {content_length, integer_to_list(erlang:iolist_size(Data))},
	    {[{content_type, "application/pdf"}, {code, 200}, Length], Data};
	{content, force_download, Data} ->
	    Length = {content_length, integer_to_list(erlang:iolist_size(Data))},
	    {[{content_type, "application/force-download"}, {code, 200}, Length], Data};
	{json, Data} ->
	    Content = e_json:encode(Data),
	    Length = {content_length, integer_to_list(length(Content))},
	    {[{content_type, "text/plain"}, {code, 200}, Length], Content};
	{template, Template} ->
	    format_response(e_mod_gen:template(Template, [],
					       e_conf:template_expander()));
	{custom, Custom} ->
	    Custom;
	{headers, Headers, NewRet} ->
	    {NewHeaders, ProperRet} = controller_exec(NewRet, View),
	    ReadyHeaders = create_headers(Headers, []),
	    {ReadyHeaders ++ NewHeaders, ProperRet};
	{error, Code} ->
	    format_response(e_mod_gen:error_page(Code, e_dict:fget("__path")))
    end.

-spec(format_response/1 :: (term()) -> term()).	     
format_response({html, HTML}) ->
    Length = {content_length, integer_to_list(erlang:iolist_size(HTML))},
    {[{content_type, "text/html"}, {code, 200}, Length], HTML};
format_response([{status, Code}, {html, HTML}]) ->
    Length = {content_length, integer_to_list(erlang:iolist_size(HTML))},
    {[{content_type, "text/html"}, {code, Code}, Length], HTML};
format_response(Else) ->
    Else.

-spec(with_formatted_error/1 :: (fun()) -> term()).	      
with_formatted_error(F) ->
    case catch F() of
	{'EXIT', Reason} ->
	    format_response(e_mod_gen:error_page(501, e_dict:fget("__path"), Reason));
	Response ->
	    format_response(Response)
    end.

-spec(create_headers/2 :: (list(tuple()), list(tuple())) -> list(tuple())).	     
create_headers([], Acc) ->
    Acc;
create_headers([{cookie, CookieName, CookieVal} | Rest], Acc) ->
    create_headers(Rest, [{"Set-cookie", CookieName ++ [$= | CookieVal ++ "; path=/"]} | Acc]);
create_headers([{cookie, CookieName, CookieVal, CookiePath} | Rest], Acc) ->
    create_headers(Rest, [{"Set-cookie", CookieName ++ [$= | CookieVal ++ "; path=" ++ CookiePath]} | Acc]);
create_headers([{cookie, CookieName, CookieVal, CookiePath, CookieExpDate} | Rest], Acc) ->
    create_headers(Rest, [{"Set-cookie", CookieName ++ [$= | CookieVal ++ "; path=" ++ CookiePath ++ "; expires=" ++ CookieExpDate]} | Acc]);
create_headers([{content_length, Len} | Rest], Acc) when is_integer(Len) ->
    create_headers(Rest, [{"Content-Length", integer_to_list(Len)} | Acc]);
create_headers([{content_length, Len} | Rest], Acc) when is_list(Len) ->
    create_headers(Rest, [{"Content-Length", Len} | Acc]);
create_headers([_ | Rest], Acc) ->
    create_headers(Rest, Acc).

%% @hidden
-spec(parse_get/1 :: (string()) -> list(tuple())).	     
parse_get(URL) ->
    fix_charset(httpd:parse_query(URL)).

-spec(parse_post/1 :: (string()) -> list(tuple())).	     
parse_post(String) ->
    case fetch_boundary(String) of
	{simple, Data} ->
	    fix_charset(httpd:parse_query(Data));
	{multipart, Boundary} ->
	    e_multipart_inets:get_multipart(String, Boundary)
    end.

%% @hidden
%% this is a quick and dirty fix, the total solution should based on 
%% "content-type=xxx;charset=yyy" property in request http header but 
%% for now, erlang only has unicode/utf8 charset decoder, so...
-spec(fix_charset/1 :: (list(tuple())) -> list(tuple())).
fix_charset(In) ->
    lists:map(fun
		  ({Key, Val}) ->
		      {Key, unicode:characters_to_list(list_to_binary(Val))}
	      end, In).

%% @hidden
-spec(fetch_boundary/1 :: (string()) -> {simple, string()} | {multipart, string()}).	     
fetch_boundary(Data) ->
    case string:str(Data, "\r\n") of
	0 -> 
	    {simple, Data};
	Pos -> 
	    {multipart, string:substr(Data, 1, Pos-1)}
    end.

%% @hidden
-spec(cookie_up/1 :: (list(tuple())) -> term()).	     
cookie_up(Headers) ->
    Cookies = get_cookies(Headers),
    eptic:fset("__cookies", Cookies),

    case proplists:get_value(?COOKIE, Cookies) of
        undefined ->
            ClientCookie = e_session:new_session([]),
            e_mod_gen:restore_session(ClientCookie),
            ClientCookie;
        ClientCookie ->
            {ok, Session} = e_session:get_session(ClientCookie),
            if
                Session == undefined ->
                    NewCookie = e_session:new_session([]),
                    e_mod_gen:restore_session(NewCookie),
                    NewCookie;
                true ->
                    e_mod_gen:restore_session(ClientCookie),
                    ClientCookie
            end
    end.

%% @hidden
-spec(cookie_bind/1 :: (string()) -> {string(), string()}).	     
cookie_bind(ClientCookie) ->
    e_mod_gen:bind_session(ClientCookie),
    {"Set-cookie", ?COOKIE ++ [$= | ClientCookie ++ "; path=/"]}.

-spec(cleanup/0 :: () -> any()).
cleanup() ->
    e_multipart_inets:terminate(),
    e_dict:terminate_state().

-spec(get_cookies/1 :: (list(tuple())) -> list(tuple())).			      
get_cookies(Headers) ->
    case lists:keysearch("cookie", 1, Headers) of
	false ->
	    [];
	{_, {_, Cookies0}} ->
	    Cookies = string:tokens(Cookies0, ";"),
	    lists:map(fun(Cookie) ->
			      list_to_tuple(string:tokens(string:strip(Cookie, both, $ ), "="))
		      end, Cookies)
    end.

get_ip({ok, {Ip, _Port}}) ->
    Ip;
get_ip(_) ->
    {error, no_ip}.
