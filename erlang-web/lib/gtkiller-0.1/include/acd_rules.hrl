-record(acd_rules, {
    id,
    acd_id,
    account_id,
    name,
    time_period,
    priority,
    active
}).

-record(acd_rules_types, {
    id = {integer, [
        primary_key,
        {private, true}
    ]},
    acd_id = {integer, [{description, "Acd id"}]},
    account_id = {integer, [{description, "Account id"}]},
    name =  {string, [
        {description, "Name"},
        {max_length, 255}
    ]},
    time_period = {string, [
        {description, "Time period"},
        {optional, "Always"}
    ]},
    priority = {integer, [{description, "Priority"}]},
    active = {string, [
        {description, "Is rule active?"},
        {optional, "Y"}
    ]}
}).