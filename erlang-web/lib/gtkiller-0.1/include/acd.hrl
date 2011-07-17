-record(acd, {
    id,
    acd_id,
    account_id,
    number,
    active
}).

-record(acd_types, {
    id = {integer, [
        primary_key,
        {private, true}
    ]},
    acd_id = {integer, [{description, "Acd id"}]},
    account_id = {integer, [{description, "Account id"}]},
    number =  {string, [
        {description, "Number"},
        {max_length, 25}
    ]},
    active = {string, [
        {description, "Is organization active?"},
        {optional, "Y"}
    ]}
}).