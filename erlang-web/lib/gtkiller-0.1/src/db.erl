-module(db).

-export([

    % migration helpers
    init/0,
    install/1,
    reset/0,

    % table/model info helpers
    get_fields_of/2,
    get_fields_of/1,
    get_model_for/1,
    get_table_info/1,
    get_fields_info/1,
    get_types_of/1,
    get_next_id_if/2,
    set_object_id/2,
    get_auth_server_node/0,
    get_attribute_index/2,

    % db queries helpers
    select/2, select/3,
    foreach/3,
    get_first/2, get_first/3,
    exec/1,
    get_table_attribute/2,

    % admin utils
    make_fake_accounts/1,
    timestamp_to_date/1,

    % util for rebuilding auto db index
    rebuild_index/0,

    % migration utils
    alter_table/1,
    get_order_predicate/2

]).

-define(TABS, [account, organization, reseller, orgadmin, device, device_line, phone_number, acd, acd_rules, acd_rule_numbers]).

%% predicate operands
%% used in generic select
-define(OP_PREDICATE, [
    {<<"greater">>, <<">">>},
    {<<"greaterOrEqual">>, <<">=">>},
    {<<"equal">>, <<"=:=">>},
    {<<"notEqual">>, <<"=/=">>},
    {<<"lessOrEqual">>, <<"=<">>},
    {<<"less">>, <<"<">>}
]).

-include("acd.hrl").
-include("acd_rules.hrl").
-include("acd_rule_numbers.hrl").
-include("account.hrl").
-include("device.hrl").
-include("device_line.hrl").
-include("reseller.hrl").
-include("organization.hrl").
-include("orgadmin.hrl").
-include("migration.hrl").

-include_lib("stdlib/include/qlc.hrl").

init() ->
    mnesia:stop(),
    Node1 = node(),
    %[Node2] = get_auth_server_node(),
    NodesList =
        case get_auth_server_node() of
            [Node2] ->
                case net_adm:ping(Node2) of
                    pong -> [Node1, Node2];
                    _    -> [Node1]
                end;
            _ -> [Node1]
        end,
    case mnesia:create_schema(NodesList) of
        ok ->
            application:start(mnesia),
            e_db:install(),

            [install(Table) || Table <- ?TABS],

            reset();

        {error, {Node, {already_exists, Node} }}  ->
            application:start(mnesia),
            error_logger:warning_msg("~p module, mnesia's schema already exists, "
                                     "if you want to delete it, run mnesia:delete_schema/1~n",
                                     [?MODULE]),
            {error, schema_already_exists}
    end.

install(Name) ->
    mnesia:create_table(Name, [{attributes, (list_to_atom("wtype_" ++ atom_to_list(Name))):get_record_info(Name)},
                               {disc_copies, [node()]}]).

get_auth_server_node() ->
    F = fun(X) ->
        case re:run(atom_to_list(X), "^olup_auth_server@.+$", [global, {capture,[1]}, {newline,any}]) of
            {match, _} -> true;
            _ -> false
        end
    end,
    case lists:filter(F, nodes()) of
        [H]    -> [H];
        [H|_T] -> [H];
        []     -> [];
        H      -> H
    end.

%% additional console helpers

reset() ->

    % rebilding db index
    rebuild_index(),

    % ser currently `ejabberd_host` config option
    % since it's the same as for olup node now
    AuthNode = list_to_atom("olup_auth_server@" ++ config:get(ejabberd_host)),

    % add auth node
    net_adm:ping(AuthNode),
    mnesia:change_config(extra_db_nodes, [AuthNode]),

    %create admin account for webaccess
    try
        e_auth:add_user("admin", "olupadmin")
    catch
        _:_ -> console:log(["Cannot add admin user"])
    end.

%%
%% gets model module
%% for mnesia table
%%
get_model_for(Table) ->
    list_to_atom("wtype_" ++ atom_to_list(Table)).

%%
%% gets fields of
%% mnesia table
%%
get_fields_of(Table) ->
    get_fields_of(Table, {exclude, []}).

get_fields_of(Table, {exclude, ExcludeFields}) ->
    (get_model_for(Table)):get_record_info(Table) -- ExcludeFields.

%%
%% gets field types of
%% mnesia table
%%
get_types_of(Table) ->
    (get_model_for(Table)):get_record_info(list_to_atom(atom_to_list(Table) ++ "_types")).

%%
%% gets attribute type
%%
get_attribute_type(Table, Attribute) ->
    Types = get_types_of(Table),
    element(1, element(get_attribute_index(Table, Attribute), Types)).

%%
%% returns value of an attribute from a tuple
%% at the type of the attribute
%%
get_attribute_index(Table, Attribute) ->
    db_auto_index:get_index(Table, Attribute).

%%
%% gets table info (fields, types)
%%
get_table_info(Table) ->
    {get_fields_of(Table), get_types_of(Table)}.

%%
%% returns TFieldDefinition information
%% fields, their types and constraints
%%
get_fields_info(Table) ->

    {Fields, Types} = get_table_info(Table),

    _NotUsed = [description, comment, private, optional],

    lists:map(fun(Field) ->

        {Type, Data} = element(get_attribute_index(Table, Field), Types),

        Constraints = [

            {struct, [
                {<<"method">>, atom_to_binary(Method, utf8)},
                {<<"data">>, utils:to_binary(Value)}
            ]}

            || {Method, Value} <- Data, not lists:member(Method, _NotUsed)
        ],

        BasicInfo = [
            {<<"name">>, atom_to_binary(Field, utf8)},
            {<<"type">>, atom_to_binary(Type, utf8)},
            {<<"constraints">>, Constraints}
        ],

        Info = if
            hd(Data) == primary_key -> BasicInfo ++ [{<<"primary">>, true}];
            true -> BasicInfo
        end,

        {struct, Info}

    end, Fields).

%%
%% generic select:
%%

%%
%% Selects all records
%% from a table
%%
select(From, all) ->
    %e_db:read(From);
    e_db_mnesia:read(From); % use e_db_mnesia to be able run from other code, where config is unavailable

%%
%% Selects key record
%% from a table
%%
select(From, Key) when is_integer(Key) ->
    %e_db:read(From, Key);
    e_db_mnesia:read(From, Key); % use e_db_mnesia to be able run from other code, where config is unavailable

%%
%% By predicate function, which
%% should return boolean to filter
%% needed records
%%
select(From, PredicateFunction) when is_function(PredicateFunction) ->
    exec(qlc:q([Record || Record <- mnesia:table(From), apply(PredicateFunction, [Record])]));

%%
%% From table, where and order
%%
select(From, [{where, Fn}, {order, {Idx, Order}}]) ->
    exec(qlc:q([R || R <- qlc:keysort(Idx, mnesia:table(From), [{order, Order}]), apply(Fn, [R])]));

%%
%% From table, where and order and limit
%%
select(From, [{where, Fn}, {order, {Idx, Order}}, {limit, {1, Length}}]) ->
    {atomic, Recs} = mnesia:transaction(fun()->
        QC = qlc:cursor(qlc:q(
            [R || R <- qlc:keysort(Idx, mnesia:table(From), [{order, Order}]), apply(Fn, [R])])),
        Ret = qlc:eval(qlc:next_answers(QC, Length)),
        qlc:delete_cursor(QC),
        Ret
    end),
    Recs;
select(From, [{where, Fn}, {order, {Idx, Order}}, {limit, {Offset, Length}}]) ->
    {atomic, Recs} = mnesia:transaction(fun()->
        QC = qlc:cursor(qlc:q(
            [R || R <- qlc:keysort(Idx, mnesia:table(From), [{order, Order}]), apply(Fn, [R])])),
        qlc:next_answers(QC, Offset - 1),
        Ret = qlc:eval(qlc:next_answers(QC, Length)),
        qlc:delete_cursor(QC),
        Ret
    end),
    Recs;
%%
%% By TFilterRules structure,
%% mostly used in JSON select requests
%%
select(From, {FilterRules, OrderByRules}) ->
    console:log(["MDEBUG: db:select -> :", From]),
    console:log(["MDEBUG: db:select FFFOOO -> :", FilterRules, OrderByRules]),
    % simple table
    SimpleTable = "mnesia:table(" ++ utils:to_list(From) ++ ")",

    %console:log(["MDEBUG: db:select SimpleTable-> :", SimpleTable]),
    %console:log(["MDEBUG: db:select obr-> :", OrderByRules]),
    %console:log(["MDEBUG: db:select gop-> :", get_order_predicate(From, OrderByRules)]),
    % table with order by
    Table = case OrderByRules of
        undefined -> SimpleTable;
        _ -> "qlc:sort(" ++ SimpleTable ++ ",[{order," ++
            get_order_predicate(From, OrderByRules) ++ "}])"
    end,

    console:log(["MDEBUG: db:select Table-> :", Table]),

    % filter part
    Filter = case FilterRules of
        undefined -> "";
        _ -> "," ++ parse_filter_rules(From, FilterRules, "")
    end,

    console:log(["MDEBUG: db:select Filter-> :", Filter]),

    % complete query string
    Query = "[R||R<-" ++ Table ++ Filter ++ "].",
    %console:log(["MDEBUG: Query:", Query]),
    exec(qlc:string_to_handle(Query, [])).

%%
%% By single attribute
%%
select(From, Attribute, Value) ->

    AttributeIdx = get_attribute_index(From, Attribute),

    try
        exec(qlc:q([X || X <- mnesia:table(From), element(AttributeIdx, X) =:= Value]))
    of
        Val -> Val
    catch
        _:_ -> false
    end.

%%
%% The same as select, but result
%% contains only needed field
%%
get_table_attribute(Table, Attribute) ->
    AttributeIdx = get_attribute_index(Table, Attribute),
    exec(qlc:q([element(AttributeIdx, X) || X <- mnesia:table(Table)])).

%%
%% the same as select, but returns
%% only first found element
%%
get_first(From, PredicateFunction) when is_function(PredicateFunction) ->
    process_get_first(select(From, PredicateFunction)).

get_first(From, Attribute, Value) ->
    process_get_first(select(From, Attribute, Value)).

%%
%% @private
%%
process_get_first(Result) ->
    case Result of
        [] -> not_found;
        _Found -> hd(Result)
    end.

%%
%% Maps by MapFunction and filters
%% by PredicateFunction records
%%
foreach(From, PredicateFunction, MapFunction)
    when is_function(PredicateFunction), is_function(MapFunction) ->
    exec(qlc:q([MapFunction(Record) || Record <- mnesia:table(From), apply(PredicateFunction, [Record])])).

%%
%% parses TFilterRule match spec
%% @private
%%
parse_filter_rules(_From, [], _M) -> "";
parse_filter_rules(_From, [D | Rules], M) ->
    MS = if
        (M == "") or (Rules == []) -> "";
        true -> " " ++ M ++ " "
    end,
    lists:flatten(["(" ++ parse_filter_rules(_From, D, M) ++ ")" ++ MS | parse_filter_rules(_From, Rules, M)]);
parse_filter_rules(From, {struct, D}, _M) ->
    Match = binary_to_list(proplists:get_value(<<"match">>, D)),
    case proplists:get_value(<<"rules">>, D) of
        undefined ->
            FieldName = binary_to_atom(proplists:get_value(<<"field">>, D), utf8),
            IdxStr = integer_to_list(db:get_attribute_index(From, FieldName)),
            Field = "element(" ++ IdxStr ++ ", R)",
            Type = get_attribute_type(From, FieldName),
            Value = jsonutils:to_value(proplists:get_value(<<"value">>, D)),
            get_match_spec(Match, Field, Type, Value);
        _Rules ->
            parse_filter_rules(From, _Rules, Match)
    end.

%%
%%
%%
timestamp_to_date(S) ->
    [D, T] = string:tokens(S, " "),
    {
        list_to_tuple([list_to_integer(X) || X <- string:tokens(D, "-")]),
        list_to_tuple([list_to_integer(X) || X <- string:tokens(T, ":")])
    }.

%%
%% @private
%%
get_match_spec(Match, Field, Type, Value) ->

    QValue = normalize_spec_value(Value),

    % for dates convert them to timestapes to be able compare
    % on contains, begins, etc filters
    SpecField = if
        Type == datetime, is_list(Value) -> "wtype_datetime:format(" ++ Field ++ ")";
        true -> Field
    end,

    %console:log(["***Field:", Field, "SpecField:", SpecField]),

    LowerQValue = string:to_lower(QValue),
    LowerFieldStr = "string:to_lower(" ++ SpecField ++ ")",
    case list_to_binary(Match) of
        <<"regexp">> -> wrap_spec("regexp:match", SpecField, QValue) ++ " =/= nomatch";
        <<"in">> -> wrap_spec("lists:member", SpecField, "[" ++ string:join(lists:map(fun(X) ->
            normalize_spec_value(jsonutils:to_value(X))
        end, Value), ",") ++ "]");
        <<"begins">> -> wrap_spec("string:str", LowerFieldStr, LowerQValue) ++ " == 1";
        <<"contains">> -> wrap_spec("string:str", LowerFieldStr, LowerQValue) ++ " /= 0";
        <<"ends">> -> wrap_spec("string:str", LowerFieldStr, LowerQValue) ++ " - length(" ++ LowerQValue ++ ") == 0";
        _Standard -> SpecField ++ binary_to_list(proplists:get_value(_Standard, ?OP_PREDICATE)) ++ QValue
    end.

%%
%% @private
%%
normalize_spec_value(Value) ->
    case Value of
        _List when is_list(Value) -> "\"" ++ Value ++ "\"";
        _Other -> utils:to_list(Value) % without quotes
    end.

%%
%% @private
%%
wrap_spec(Wrapper, Field, Value) ->
    Wrapper ++ "("  ++ Field ++ ", " ++ Value ++ ")".

%%
%% parse orderBy structure,
%% returns predicate function for qlc:sort
%%
get_order_predicate(Table, OrderByList) ->
    "fun " ++ string:join(lists:reverse(get_order_rules(Table, OrderByList, [])), ";") ++ " end".

%%
%% parse orderBy rules,
%% @private
%%
get_order_rules(_, [], _) -> "";
get_order_rules(Table, [{struct, OrderByRule} | OrderByList], PreviousRules) ->
    %console:log(["MDEBUG: db:get_order_rules 1-> :", Table]),
    %console:log(["MDEBUG: db:get_order_rules 2-> :", OrderByRule]),
    Field = binary_to_atom(proplists:get_value(<<"field">>, OrderByRule), utf8),
    %console:log(["MDEBUG: db:get_order_rules 3-> :", Field]),
    Order = case proplists:get_value(<<"order">>, OrderByRule) of
        <<"asc">> -> "=<";
        <<"desc">> -> ">";
        undefined -> "=<" % asc
    end,
    %console:log(["MDEBUG: db:get_order_rules 4-> :", Order]),
    Idx = integer_to_list(db:get_attribute_index(Table, Field)),
    %console:log(["MDEBUG: db:get_order_rules 5-> :", Idx]),
    {El1, El2} = get_order_elements_for_index(Idx),
    %console:log(["MDEBUG: db:get_order_rules 6-> :", El1, El2]),
    Guard = case PreviousRules of
        [] -> "";
        _ -> " when " ++ string:join([P1 ++ "=:=" ++ P2 || {P1, P2} <- PreviousRules], ",")
    end,
    %console:log(["MDEBUG: db:get_order_rules 7-> :", Guard]),
    ["(A,B)" ++ Guard ++ "->" ++ El1 ++ Order ++ El2 | get_order_rules(Table, OrderByList, PreviousRules ++ [{El1, El2}])].

%%
%% @private
%%
get_order_elements_for_index(Idx) ->
    {"element(" ++ Idx ++ ",A)",
    "element(" ++ Idx ++ ",B)"}.

get_next_id_if(Table, Row) ->
    Id = element(2, Row),
    case Id of
        undefined -> e_db:get_next_id(Table);
        _string when is_list(Id) -> list_to_integer(Id);
        _ -> Id
    end.

set_object_id(Table, Row) ->
    NextId = get_next_id_if(Table, Row),
    setelement(2, Row, NextId).

% qcl query helper
exec(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

make_fake_accounts(HowMany) ->
    lists:foreach(fun(N) ->
        ListN = integer_to_list(N),
        wtype_account:create(#account{
            id = undefined,
            email = "u" ++ ListN ++ "@uuuuu.com",
            name = "U-" ++ ListN,
            password = "1"
        })
    end, lists:seq(1, HowMany)).

%%
%% Rebuild database index for
%% optimized selects
%%
rebuild_index() ->

    Dir = "lib/gtkiller-0.1/",

io:format("rebuild_index ~n",[]),

    % source list
    SourceList = lists:map(fun(TableAtom) ->
        Table = atom_to_list(TableAtom),
        % get syntax tree
        {ok, Tree} = epp:parse_file(Dir ++ "include/" ++ Table ++ ".hrl", ["./"], []),
         % at offset 2 - our record info
        {attribute, _, record, {_RecordName, RecordData}} = lists:nth(2, Tree),
        % generate the source for current [table:field:index]
        ["\n%% " ++ Table ++ " index"] ++
        [
            "\nget_index(" ++ Table ++ ", " ++ atom_to_list(Field) ++ ") -> " ++ integer_to_list(Index) ++ ";" ||
            {record_field, Index, {atom, _Index, Field}} <- RecordData
        ]
    end, ?TABS),

io:format("rebuild_index after source lists ~n",[]),

    % join the list to the source string and replace the last ";" with "."
    Source = string:strip(lists:flatten(string:join(SourceList, "\n")), right, $;) ++ ".",

    % the name of auto indexes module
    ModuleName = "db_auto_index",

    % module info
    Header = "%% This module automatically generated - *do not edit*\n"++
    	"%% Provides database indexes to get the direct offset in dynamic queries\n"++
    	"\n"++
    	"-module(" ++ ModuleName ++ ").\n"++
    	"-export([get_index/2]).\n"++
    	"\n",

    file:write_file(Dir ++ "src/" ++ ModuleName ++ ".erl", list_to_binary(Header ++ Source)).

%%
%% Executes migration for
%% a table (migration.hrl record)
%%
alter_table([TableName]) ->

    Table = list_to_atom(TableName),

    error_logger:logfile({open, "log/migration.log"}),

    error_logger:tty(true),

    mnesia:start(),
    mnesia:wait_for_tables([Table], 20000),
    mnesia:info(),

    error_logger:info_msg("Migration of: ~p~n", [Table]),

    OldFields = db:get_fields_of(Table),
    error_logger:info_msg("OldFields: ~p~n", [OldFields]),

    NewFields = record_info(fields, migration@),
    error_logger:info_msg("NewFields: ~p~n", [NewFields]),

    NewFieldTypes = #migration@_types{},
    error_logger:info_msg("NewFieldTypes: ~p~n", [NewFieldTypes]),

    NewIdxFieldMap = lists:zip(NewFields, lists:seq(2, length(NewFields) + 1)),
    error_logger:info_msg("NewIdxFieldMap: ~p~n", [NewIdxFieldMap]),

    OldIdxFieldMap = lists:zip(OldFields, lists:seq(2, length(OldFields) + 1)),
    error_logger:info_msg("OldIdxFieldMap: ~p~n", [OldIdxFieldMap]),

    TransformResult = mnesia:transform_table(
        Table,
        fun(OldRecord) ->
            error_logger:info_msg("OldRecord: ~p~n", [OldRecord]),
            NewRecord = list_to_tuple([Table] ++ lists:map(fun({NewField, NewIndex}) ->
                % try to get an index of the field in the old set
                case proplists:get_value(NewField, OldIdxFieldMap) of
                    % for new field use default value
                    undefined ->
                        TypeData = element(NewIndex, NewFieldTypes),
                        TypeSpec = element(2, TypeData),
                        error_logger:info_msg("TypeSpec: ~p~n", [TypeSpec]),
                        case lists:keysearch(optional, 1, TypeSpec) of
                            {value, {optional, Default}} ->
                                error_logger:info_msg("Default: ~p~n", [Default]),
                                Default;
                            _ ->
                                undefined
                        end;
                    % otherwise copy old value from the record
                    OldIndex -> element(OldIndex, OldRecord)
                end
            end, NewIdxFieldMap)),
            error_logger:info_msg("NewRecord: ~p~n", [NewRecord]),
            NewRecord
        end,
        NewFields
    ),

    error_logger:info_msg("TransformResult: ~p~n", [TransformResult]),
    TransformResult.
