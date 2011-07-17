-module(controller_admin).

-extends(controller_base).

-export([
    login/0, login/1,
    logout/0,

    % direct actions
    handle_login/1,
    handle_logout/1
]).

-include("constants.hrl").
-include("utils_controller_annotations.hrl").

%%
login() ->
    Command = wpart:fget(?KEY_COMMAND_JSON),

    Login = jsonutils:get_attribute(Command, ?TO_JSON_NAME(login)),
    Password = jsonutils:get_attribute(Command, ?TO_JSON_NAME(password)),

    console:log(["ADMIN login: -> ", Login, Password]),

    case utils_auth:login(Login, Password) of
        ok ->
            console:log(["About:", wpart:fget("session:admin_about2see"), wpart:fget("session:about2see")]),
            case wpart:fget("session:admin_about2see") of
                undefined -> [];
                Url ->
                    wpart:fdelete("session:about2see"),
                    [{?TO_JSON_NAME(requestedURL), list_to_binary(Url)}]
            end;
        {error, _Reason} ->
	    console:log(["ADMIN login failed: -> ", Login, Password]),
            utils_error:insert(?ERROR_USER_NOT_FOUND),
            ?MODULE:login(error)
    end.

login(error) ->
    ?MODULE:default(error).

%%
logout() ->
    utils_auth:logout_admin(),
    [].

%%
handle_login(_Arguments) ->
    {template, "main.html"}.

%%
handle_logout(_Arguments) ->
    utils_auth:logout_admin(),
    {redirect, "/home"}.

