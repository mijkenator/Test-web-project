-record(account, {
    id,
    organization_id,
    name,
    password,
    email,
    active,
    jid,
    status,
    remind_code,
    need_new_password,
    created,
    login,
    client_version,
    last_login,
    avatar
}).

-record(account_types, {
    id = {integer, [
        primary_key,
        {private, true}
    ]},
    organization_id = {integer, []},
    name = {string, [
        {description, "Name"},
        {max_length, 256},
        {min_length, 1}
    ]},
    password = {string, [
        {description, "Password"},
        {max_length, 20}
    ]},
    email = {string, [
        {description, "E-mail"},
        unique,
        {regexp, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}"},
        {max_length, 50}
    ]},
    active = {flag, [
        {description, "Is account active?"},
        {optional, true}
    ]},
    % hiden field based on email (replace "@" on "-at-")
    jid = {string, [
        {description, "Ejabberd User Name"},
        {private, true}
    ]},
    status = {string, [
        {description, "Custom status"},
        {optional, ""}
    ]},
    remind_code = {string, [
        {description, "Remind code"},
        {optional, ""}
    ]},
    need_new_password = {flag, [
        {description, "Should a user replace automatically generated password?"},
        {optional, false}
    ]},
    created = {datetime, [
        {description, "Created datetime"},
        {comment, "Format: YYYY-MM-DD HH:NN:SS"},
        {format, "YYYY-MM-DD HH:NN:SS"},
        {private, true}
    ]},
    login = {string, [
        {description, "Login"},
        unique,
        {regexp, "[a-zA-Z0-9._%+-]{1,255}"}
    ]},
    client_version = {string, [
        {description, "Client Version"},
        {optional, "1.0.0.1"}
    ]},
    last_login = {datetime, [
        {description, "Last login"},
        {comment, "Format: YYYY-MM-DD HH:NN:SS"},
        {format, "YYYY-MM-DD HH:NN:SS"},
        {optional, {{0000,00,00}, {00,00,00}}},
        {private, true}
    ]},
    avatar = {string, [
        {description, "avatar"},
        {optional, ""}
    ]}
}).
