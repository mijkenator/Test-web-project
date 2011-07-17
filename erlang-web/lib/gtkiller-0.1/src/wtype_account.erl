-module(wtype_account).

-export([
    get_record_info/1,

    % CRUD
    create/1,
    read/1,
    update/1,
    delete/1,

    set_custom_status/1,

    prepare_initial/0,
    prepare_validated/0,
    prepare_edit/1,

    email_to_jid/1,

    get_sessioned_account_structure/0,

    to_full_jid/1,
    get_today_registered_users_number/0
]).

-include("account.hrl").

get_record_info(account) -> record_info(fields, account);
get_record_info(account_types) -> #account_types{}.

create(AccountArg) ->

    console:log(["WAC1"], 'DEBUG'),
    % new autoincremented id
    Id = db:get_next_id_if(account, AccountArg),
    console:log(["WAC2"], 'DEBUG'),
    % prepopulated jid
    Jid = generate_jid(AccountArg#account.login, Id),
    console:log(["WAC3"], 'DEBUG'),
    % default fields
    Account = AccountArg#account{
        id = Id,
        jid = Jid,
        created = calendar:universal_time(),
        last_login = {{0,0,0}, {0,0,0}}
    },
    console:log(["WAC4"], 'DEBUG'),
    console:log(["Creating account:", Account]),

    % add to accounts table
    e_db:write(account, Account),
    console:log(["WAC5"], 'DEBUG'),
    Account.


read(all) ->
    e_db:read(account);
read(Id) ->
    e_db:read(account, Id).

update(Account) ->

    console:log(["Accoun UPDATE:", Account]),

    e_db:update(account, Account),

    Account.

delete(Id) ->

    % remove from accounts table
    e_db:delete(account, Id).

%%
%% Handles offline custom status
%%

set_custom_status(Status) ->

    Account = wpart:fget("session:account"),

    %% update account
    update(Account#account{status = Status}).

prepare_initial() ->
    wpart_db:build_record_structure(account, #account{}).

prepare_validated() ->
    Account = wpart:fget("__not_validated"),
    wpart_db:build_record_structure(account, Account).

prepare_edit(Item) ->
    wpart_db:build_record_structure(account, Item).

%%
email_to_jid(Email) ->
    re:replace(Email, "@", "-at-", [{return, list}]).

get_sessioned_account_structure() ->
    Format = fun(X) ->
        utils:format(account, X)
    end,
    InitData = case wpart:fget("session:admin") of
        undefined -> [];
		Admin -> [{<<"admin">>, Format(Admin)}]
	end,
    Data = case wpart:fget("session:account") of
        undefined -> InitData;
		Account -> InitData ++ [{<<"user">>, Format(Account)}]
	end,
    {struct, Data}.

generate_jid(Login, Id) ->
    Jid = to_full_jid(Login),
    case db:get_first(account, jid, Jid) of
        not_found -> Jid;
        _Account  -> to_full_jid(Login ++ integer_to_list(Id))
    end.

to_full_jid(PartJid) ->
    case PartJid of
        undefined -> undefined;
        Jid -> case string:str(Jid, "@") of
            0 -> Jid ++ "@" ++ config:get(ejabberd_host);
            _AlreadyFullJid -> Jid
        end
    end.

%%
%% per last 24 hours:
%% number of registered users
%%
get_today_registered_users_number() ->
    length(db:select(account, fun(R) ->
        Yesterday = calendar:gregorian_seconds_to_datetime(
            calendar:datetime_to_gregorian_seconds({date(), time()}) - (24 * 60 * 60)
        ),
        console:log(["R#account.created:", R#account.created, "Yesterday:", Yesterday]),
        R#account.created >= Yesterday
    end)).

