-record(device_line, {
    id,
    device_id,
    name,
    auth_name,
    auth_passwd,
    proxy_addr,
    proxy_port
}).

-record(device_line_types, {
    id = {integer, [
        primary_key,
        {private, true}
    ]},
    device_id = {integer, [{description, "Device id"}]},
    name = {string, [
        {description, "Line name"},
        {max_length, 256},
        {min_length, 1}
    ]},
    auth_name = {string, [
        {description, "Auth name"},
        {max_length, 256},
        {min_length, 1}
    ]},
    auth_passwd = {string, [
        {description, "Auth Password"},
        {max_length, 20}
    ]},
    proxy_addr = {string, [
        {description, "Proxy address"},
        {max_length, 256}
    ]},
    proxy_port =  {string, [
        {description, "Proxy port"},
        {max_length, 256}
    ]},
    active = {string, [
        {description, "Is organization active?"},
        {optional, "Y"}
    ]}
}).
