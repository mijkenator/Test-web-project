-record(reseller, {
    id,
    name,
    password,
    email,
    active,
    info,
    created
}).

-record(reseller_types, {
    id = {integer, [
        primary_key,
        {private, true}
    ]},
    name = {string, [
        {description, "Organization name"},
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
    active = {string, [
        {description, "Is organization active?"},
        {optional, "Y"}
    ]},
    info = {string, [
        {description, "Organization name"},
        {max_length, 1024},
        {optional, " "}
    ]},
    created = {datetime, [
        {description, "Created datetime"},
        {comment, "Format: YYYY-MM-DD HH:NN:SS"},
        {format, "YYYY-MM-DD HH:NN:SS"},
        {private, true}
    ]}
}).
