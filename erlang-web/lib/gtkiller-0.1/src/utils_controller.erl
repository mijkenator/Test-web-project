-module(utils_controller).

-export([
    process_request/4,
    process_response/4,
    prepare_data/4,
    validate_data/4,
    check_existence/4,
    authorize/4,
    direct_action/4,
    test/4
]).
-export([
    field_to_property/2, property_to_field/2,
    get_errors/0, clear_errors/0, add_error/1
]).

-include("lib/eptic-1.4.1/include/e_annotation.hrl").
-include("constants.hrl").

%%%
%%% Annotations.
%%%

%%
?BEFORE.
process_request(_Argument, _Module, _Function, Arguments) ->
    Type = string:substr(atom_to_list(hd(Arguments)), 12),
    Request = wpart:fget("post:request"),
    console:log(["Request:", Request]),
    case Request of
        % Not-JSON request: return template.
        undefined ->
            {skip, {template, lists:append(Type, "/index.html")}};
        % JSON request: process request.
        _ ->
            JSON = mochijson2:decode(Request),
            console:log(["Request JSON:", JSON], 'DEBUG'),
            wpart:fset("_request", JSON),
            {proceed, Arguments}
    end.

%%
?AFTER.
process_response(_Argument, _Module, _Function, Result) ->
    JSON = wpart:fget("_response"),
    case JSON of
        % Not-JSON response: nothing to do.
        undefined ->
            {skip, Result};
        % JSON response: process response.
        _ ->
            Response = lists:flatten(mochijson2:encode(JSON)),
            console:log(["Response JSON:", Response], 'DEBUG'),
            {proceed, {content, text, Response}}
    end.

%%
?BEFORE.
prepare_data(_, Module, Function, Arguments) ->
    case register_annotation(prepare_data) of
        true ->
            Command  = wpart:fget(?KEY_COMMAND_JSON),
            Type     = Module:get_type(),
            PostItem = utils_validator:prepare_data(Type, Function, Command),

            % save raw item
            wpart:fset(?KEY_COMMAND_RAW_DATA, PostItem),

            % merge real post item with already
            % existing in db, to be able pass
            % not all fields on update
            Item = case wpart:fget(?KEY_COMMAND_ID) of
                undefined -> PostItem;
                Id ->
                    FromUnicode = fun(X) ->
                        case X of
                            _String when is_list(_String) -> binary_to_list(unicode:characters_to_binary(X));
                            _ -> X
                        end
                    end,
                    PreviousItem = list_to_tuple([FromUnicode(Field) || Field <- tuple_to_list(db:select(Type, Id))]),
                    wpart:fset(?KEY_COMMAND_PREVIOUS_DATA, PreviousItem),
                    utils:update_tuple_elements(PreviousItem, PostItem)
            end,

            wpart:fset(?KEY_COMMAND_DATA, Item),
            {proceed, Arguments};
        false ->
            {proceed, Arguments}
    end.

%%
?BEFORE.
validate_data(_, Module, Function, Arguments) ->
    console:log(["VALIDATE DATA:", Function, Arguments]),
    case register_annotation(validate_data) of
        true ->
	    console:log(["VALIDATE DATA:1"]),
            Data = wpart:fget(?KEY_COMMAND_DATA),
            case utils_validator:validate_data(Data) of
                {ok, Item} ->
		    console:log(["VALIDATE DATA:2"]),
                    wpart:fset(?KEY_COMMAND_DATA, Item),
                    {proceed, Arguments};
                {error, Errors} ->
		    console:log(["VALIDATE DATA:3"]),
                    Callback = fun(Error) ->
                        DescriptionAton = element(2, Error),
                        ErrorCode = case DescriptionAton of
                            element_not_unique -> ?ERROR_DUPLICATE_PRIMARY_KEY;
                            _Other -> ?ERROR_DATA_NOT_VALID
                        end,
                        Field       = {field, atom_to_list(element(1, Error))},
                        Description = {description, DescriptionAton},
                        utils_error:insert({ErrorCode, [Field, Description]})
                    end,
                    lists:foreach(Callback, Errors),
                    {error, {Module, Function, [error]}}
            end;
        false ->
	    console:log(["VALIDATE DATA:4"]),
            {proceed, Arguments}
    end.

%%
?BEFORE.
check_existence(_, Module, Function, Arguments) ->
    case register_annotation(check_existence) of
        true ->
            Command = wpart:fget(?KEY_COMMAND_JSON),
            Id      = utils:to_integer(jsonutils:get_attribute(Command, ?JSON_ID)),
            Type    = Module:get_type(),
            Model   = db:get_model_for(Type),
            case Model:read(Id) of
                not_found ->
                    utils_error:insert({?ERROR_DATA_NOT_VALID, [{description, item_not_exists}]}),
                    {error, {Module, Function, [error]}};
                _ ->
                    wpart:fset(?KEY_COMMAND_ID, Id),
                    {proceed, Arguments}
            end;
        false ->
            {proceed, Arguments}
    end.

%%
?BEFORE.
authorize(Format, Module, Function, Arguments) ->

    AccountId = wpart:fget("session:account_id"),
    Admin = wpart:fget("session:admin"),
    VideomailAuth = wpart:fget("session:videomail_auth"),

    console:log(["**AccountId:", AccountId]),
    console:log(["**Format:", Format]),
    console:log(["**Admin:", Admin]),
    console:log(["**VideomailAuth:", VideomailAuth]),

    case Format of
        admin ->
            case Admin of
            undefined ->
                console:log(["Admin not logged in"]),
                wpart:fset("session:admin_about2see", wpart:fget("__path")),
                {skip, {redirect, "/admin"}};
            _AdminIsLoggedIn ->
                console:log(["***Admin logged in"]),
                {proceed, Arguments}
            end;
        normal ->
            case AccountId of
            undefined ->
                console:log(["User not logged in"]),
                wpart:fset("session:about2see", wpart:fget("__path")),
                {skip, {redirect, "/"}};

            _UserLoggedIn ->
                {proceed, Arguments}
            end;
        json ->
            console:log(["JSON Format"]),
            if (AccountId /= undefined) or (Admin /= undefined) ->
                console:log(["OK; JSON login; Being on path:", wpart:fget("__path")]),
                {proceed, Arguments};
            true ->
                console:log(["***Error; JSON login; Being on path:", wpart:fget("__path")]),
                utils_error:insert(?ERROR_AUTH_REQUIRED),
                {error, {Module, Function, Arguments ++ [error]}}
            end;
        videomail ->
            % for videomail use special per-page auth
            % without a real authorization
            if (AccountId /= undefined) or (VideomailAuth /= undefined) ->
                % if there is no flag
                {proceed, Arguments};
            true ->
                utils_error:insert(?ERROR_AUTH_REQUIRED),
                {error, {Module, Function, Arguments ++ [error]}}
            end
    end.

%%
?BEFORE.
direct_action(_Arg, _Module, _Function, Arguments) ->
    {proceed, Arguments}.

%%
?BEFORE.
test(Argument, Module, Function, Arguments) ->
    console:log(["Annotation 'TEST':", {Argument, Module, Function, Arguments}], 'DEBUG'),
    {proceed, Arguments}.

%%%
%%% Utils.
%%%

%%
register_annotation(Annotation) ->
    Annotations = case wpart:fget(?KEY_COMMAND_ANNOTATIONS) of
        undefined ->
            [];
        Result ->
            Result
    end,
    case lists:member(Annotation, Annotations) of
        true ->
            false;
        false ->
            wpart:fset(?KEY_COMMAND_ANNOTATIONS, [Annotation | Annotations]),
            true
    end.

%%
field_to_property(Type, Field) when is_atom(Type) ->
    field_to_property(atom_to_list(Type), Field);
field_to_property(Prefix, Field) ->
    lists:nthtail(length(Prefix) + 1, Field).

%%
property_to_field(Type, Property) when is_atom(Type) ->
    property_to_field(atom_to_list(Type), Property);
property_to_field(Prefix, Property) ->
    lists:append([Prefix, "_", Property]).

%%
get_errors() ->
    case wpart:fget("_errors") of
        undefined ->
            [];
        Errors ->
            Errors
    end.

%%
clear_errors() ->
    wpart:fset("_errors", []).

%%
add_error(Error) ->
    wpart:fset("_errors", get_errors() ++ [Error]).
