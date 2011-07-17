-module(config).
-export([
    get/1,
    as_json_struct/0
]).

-define(JSON_EXPORT_KEYS, [ejabberd_host, server_version, ping_timeout, wall_enabled, avatars_directory]).

-include("account.hrl").

get(Key) ->
    case e_conf:get_conf(Key) of
        undefined ->
            console:log(["Value for config key is not found:", Key]),
            undefined;
        Value -> Value
    end.
    
as_json_struct() ->
    ConfigOptions = lists:zip(
        ?JSON_EXPORT_KEYS,
        [
            utils:to_binary(config:get(X)) ||
            X <- ?JSON_EXPORT_KEYS
        ]
    ),
        
    { struct, ConfigOptions }.
