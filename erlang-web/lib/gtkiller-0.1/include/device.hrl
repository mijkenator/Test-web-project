-record(device, {
    id,
    organization_id,
    account_id,
    name,
    password,
    type,
    mac,
    active,
    info,
    created
}).

-record(device_types, {
    id = {integer, [
        primary_key,
        {private, true}
    ]},
    organization_id = {integer, [{description, "Organization id"}]},
    account_id = {integer, [{description, "Account id"}, {optional, 0}]},
    name = {string, [
        {description, "Device name"},
        {max_length, 256},
        {min_length, 1}
    ]},
    password = {string, [
        {description, "Password"},
        {max_length, 20}
    ]},
    type = {string, [
        {description, "Pbx device type"},
        {max_length, 256}
    ]},
    mac =  {string, [
        {description, "Mac address"},
        {max_length, 256}
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
