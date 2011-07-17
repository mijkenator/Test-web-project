-record(organization, {
    id,
    name,
    reseller_id,
    active,
    created
}).

-record(organization_types, {
    id = {integer, [
        primary_key,
        {private, true}
    ]},
    name = {string, [
        {description, "Organization name"},
        {max_length, 256},
        {min_length, 1}
    ]},
    reseller_id = {integer, [
        {description, "Reseller id (owner of the organization)"}
    ]},
    active = {string, [
        {description, "Is organization active?"},
        {optional, "Y"}
    ]},
    created = {datetime, [
        {description, "Created datetime"},
        {comment, "Format: YYYY-MM-DD HH:NN:SS"},
        {format, "YYYY-MM-DD HH:NN:SS"},
        {private, true}
    ]}
}).
