-record(phone_number, {
    id,
    organization_id,
    account_id,
    number,
    type,
    active,
    info,
    created
}).

-record(phone_number_types, {
    id = {integer, [
        primary_key,
        {private, true}
    ]},
    organization_id = {integer, [{description, "Organization id"}]},
    account_id = {integer, [{description, "Account id"}, {optional, 0}]},
    number = {string, [
        {description, "Phone number"},
        {max_length, 256},
        {min_length, 1}
    ]},
    type = {string, [
        {description, "Phone number type"},
        {optional, "extension"}
    ]},
    active = {string, [
        {description, "Is extension active?"},
        {optional, "Y"}
    ]},
    info = {string, [
        {description, "Extension info"},
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
