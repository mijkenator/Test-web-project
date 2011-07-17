-record(acd_rule_numbers, {
    id,
    acd_rule_id,
    order,
    forward_type,
    number,
    timeout,
    active
}).

-record(acd_rule_numbers_types, {
    id = {integer, [
        primary_key,
        {private, true}
    ]},
    acd_rule_id = {integer, [{description, "Acd rulel id"}]},
    order = {integer, [{description, "Order"}]},
    forward_type =  {string, [
        {description, "Forward type"},
        {max_length, 25}
    ]},
    number =  {string, [
        {description, "Number"},
        {max_length, 25}
    ]},
    timeout = {integer, [{description, "Time out"}]},
    active = {string, [
        {description, "Is organization acd rule?"},
        {optional, "Y"}
    ]}
}).