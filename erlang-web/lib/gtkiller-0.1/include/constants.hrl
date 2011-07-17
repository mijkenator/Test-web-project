-define(ERROR_UNKNOWN,                  '0').
-define(ERROR_DATABASE_FAILED,          '100').
-define(ERROR_COMMAND_UNDEFINED,        '101').
-define(ERROR_COMMAND_UNKNOWN,          '102').
-define(ERROR_USER_NOT_FOUND,           '103').
-define(ERROR_USER_INACTIVE,            '104').
-define(ERROR_DATA_NOT_VALID,           '105').
-define(ERROR_AUTH_REQUIRED,            '106').
-define(ERROR_BAD_COMMAND_STRUCTURE,    '107').
-define(ERROR_DUPLICATE_PRIMARY_KEY,    '108').
-define(ERROR_RECORD_LOST,              '109').
-define(ERROR_FORBIDDEN,                '110').
-define(ERROR_EMAIL_WASNT_SENT,         '111').
-define(ERROR_ACTION_FAILD,             '112').
-define(ERROR_INSTALLER_NOT_FOUND,      '113').
-define(ERROR_BAD_CHARACTERS,           '114').

-define(KEY_COMMAND,               "_command").
-define(KEY_COMMAND_JSON,          "_command:json").
-define(KEY_COMMAND_ID,            "_command:id").
-define(KEY_COMMAND_DATA,          "_command:data").
-define(KEY_COMMAND_RAW_DATA,      "_command:raw_data").
-define(KEY_COMMAND_PREVIOUS_DATA, "_command:previous_data").
-define(KEY_COMMAND_ERRORS,        "_command:errors").
-define(KEY_COMMAND_ANNOTATIONS,   "_command:annotations").
-define(KEY_REQUEST,               "_request").
-define(KEY_RESPONSE,              "_response").

-define(TO_JSON_NAME(Name), atom_to_binary(Name, utf8)).
-define(JSON_CODE,        <<"code">>).
-define(JSON_COMMANDS,    <<"commands">>).
-define(JSON_DATA,        <<"data">>).
-define(JSON_DESCRIPTION, <<"description">>).
-define(JSON_ERRORS,      <<"errors">>).
-define(JSON_ID,          <<"id">>).
-define(JSON_TYPE,        <<"type">>).


% Request methods.
-define(METHOD_GET,  "GET").
-define(METHOD_POST, "POST").

-define(STAT_LOG(Method, Args), rpc:call(e_conf:get_conf(ustatd_node), ustatd, Method, Args)).