-module(index).
-export([public/1, admin/1, user/1, index/1, adminp/1, admin_acc_phg/1, acdrules/1]).

-include("account.hrl").
-include("utils_controller_annotations.hrl").

index(_Args) ->
    render("index").

public(_Args) ->

    % current page/path
    Path = case wpart:fget("__path") of
        "" -> "home";
        OtherPath -> OtherPath
    end,

    % handler specific case with "change-password/key"
    ChangePasswordPath = "change-password",
    Page = case string:str(Path, ChangePasswordPath) of
        1 -> ChangePasswordPath;
        0 -> Path
    end,

    % set pages content info
    lists:foreach(
        fun(Part) ->
            FilePath = "templates/pages/public/" ++ Page ++ "/" ++ Part ++ ".html",
            {ok, BinaryContent} = file:read_file(FilePath),
            wpart:fset("page_" ++ Part, binary_to_list(BinaryContent))
        end,
        ["body", "head", "title", "robots", "description", "keywords"]
    ),

    % script info
    wpart:fset("page_script",
        "<script type=\"text/javascript\" src=\"/pages/public/" ++ Page ++ "/" ++ Page ++ ".js\"></script>"),

    render("public").

?AUTHORIZE(normal).
user(_Args) ->
    render("user").

?AUTHORIZE(admin).
admin(_Args) ->
    console:log(["ADMIN RENDER: ", wpart:fget("__path")]),
    render("admin").
    
?AUTHORIZE(admin).
adminp(_Args) ->
    console:log(["ADMINP RENDER:", wpart:fget("__path")]),
    wpart:fset("current_device_id", proplists:get_value(id, _Args)),
    render("adminp").
    
?AUTHORIZE(admin).
admin_acc_phg(_Args) ->
    AccountId = proplists:get_value(id, _Args),
    console:log(["ACCPHG1:", AccountId]),
    Account = wtype_account:read(list_to_integer(AccountId)),
    console:log(["ACCPHG2:", Account]),
    wpart:fset("current_account_id", AccountId),
    wpart:fset("current_organization_id", integer_to_list(element(3,Account))),
    render("admin_acc_phg").
    %render("adminp").
    
?AUTHORIZE(admin).
acdrules(_Args) ->
    AccountId = proplists:get_value(accid, _Args),
    AcdId     = proplists:get_value(acdid, _Args),
    console:log(["AccID AcdID:", AccountId, AcdId ]),
    {template, "pages/admin/acdrules/acdrules.html"}.

render(Tpl) ->
    wpart:fset("account_info", jsonutils:encode(wtype_account:get_sessioned_account_structure())),
    {template, "pages/" ++ Tpl ++ "/" ++ Tpl ++ ".html"}.
