-module(utils_auth).

-export([
    login/2,
    logout/0,
    logout_admin/0
]).
-export([get_account/0, get_account_jid/0, set_account_data/1, set_admin_data/0]).

-include("account.hrl").


%%%
%%% Constants.
%%%

-define(KEY_SESSION_ACCOUNT, "session:account").

%%%
%%% Functions.
%%%

%%
login(Login, Password) ->
    console:log(["***LOGIN | Login:", Login, "Password:", Password]),
    case e_auth:login(Login, Password) of
        {ok, UserType} ->
            console:log(["Login OK!!"]),
            case UserType of
                admin ->
                    console:log(["Admin logged in."]),
                    set_admin_data();
                Account ->
                    console:log(["User logged in."]),
                    wpart:fdelete("session:account_id"),
                    wpart:fdelete(?KEY_SESSION_ACCOUNT),
                    set_account_data(Account)
            end,
    console:log(["utils auth LOGIN OK"]),
            ok;
        Result ->
            console:log(["Login failed!!"]),
            Result
    end.

%%
logout() ->
    wpart:fdelete("session:account_id"),
    wpart:fdelete("session:about2see"),
    wpart:fdelete(?KEY_SESSION_ACCOUNT),
    e_auth:logout().

logout_admin() ->
    wpart:fdelete("session:admin"),
    wpart:fdelete("session:admin_about2see"),
    case wpart:fget(?KEY_SESSION_ACCOUNT) of
        undefined -> e_auth:logout();
        _AccountIsLoggedIn -> ok
    end.

%%
get_account() ->
    wpart:fget(?KEY_SESSION_ACCOUNT).

%%
get_account_jid() ->
    case get_account() of
        undefined ->
            undefined;
        Account ->
            Account#account.jid
    end.

%%
set_account_data(Account) ->
    wpart:fset(?KEY_SESSION_ACCOUNT, Account),
    wpart:fset("session:account_id", Account#account.id),
    ok.

set_admin_data() ->
console:log(["SAD: 1"]),
    wpart:fset("session:admin", #account{
        id = 0,
        name = "Admin",
        password = "",
        email = "admin@unison.com",
        active = true,
        login = "admin"
    }),
console:log(["SAD: 2"]),
ok.