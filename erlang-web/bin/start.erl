#!/usr/bin/env escript

%% The contents of this file are subject to the Erlang Web Public License,
%% Version 1.0, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Web Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang-consulting.com/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Erlang Training & Consulting
%% Ltd. Portions created by Erlang Training & Consulting Ltd are Copyright 2009,
%% Erlang Training & Consulting Ltd. All Rights Reserved.

%%%-----------------------------------------------------------------------------
%%% File    : start.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang.consulting.com>
%%% @doc A start script for the Erlang Web framework.
%%% @end
%%%-----------------------------------------------------------------------------

-include_lib("kernel/include/file.hrl").

main(["ewgi_inets"]) ->
    start(ewgi_inets);
main(["yaws"]) ->
    start(yaws);
main(_) ->
    start(inets).

start(Server) ->
    create_start_dirs(),
    RootDir = prepare_paths(),

    Info = get_info(),
    copy_bin_files(Info),

    create_start_scripts(Info),
    create_start_erl_data(Info),
    
    create_basic_config_files(),
    create_welcome_page(),
    
    create_rel_file(Info, Server),

    generate_boot_file(),
    copy_conf_files(),
    create_sys_config_file(Server),
    
    copy_escripts(RootDir).

confirm_created(Name) ->
    io:format("Element created: ~s~n", [Name]).

inform_exists(Name) ->
    io:format("Element exists, skipping: ~s~n", [Name]).

handle_error(Reason) ->
    io:format("An error has occured: ~p~n", [Reason]).

create_start_dirs() ->
    Creator = fun(X) ->
		      case file:make_dir(X) of 
			  ok -> confirm_created(X);
			  {error, eexist} -> inform_exists(X);
			  {error, Reason} -> handle_error(Reason)
		      end
	      end,
    Dirs = ["config", "docroot", "log", "pipes", "templates", "bin",
	    "lib", "releases", filename:join("releases", "0.1"),
	    filename:join("templates", "cache"),
	    filename:join("docroot", "conf")],
    lists:foreach(Creator, Dirs),
    
    {ok, Apps1} = file:list_dir("lib"),
    Apps = lists:delete(".svn", Apps1),
    LibDirs = lists:map(fun(X) ->
				filename:join("lib", X) 
			end, Apps),
    AppDirs = ["doc", "ebin", "include", "priv", "src"],
    lists:foreach(fun(Lib) ->
			  AppDirsComplete = lists:map(fun(Name) ->
							      filename:join(Lib, Name)
						      end, AppDirs),
			  lists:foreach(Creator, AppDirsComplete)
		  end, LibDirs).

prepare_paths() ->
    ScriptName = escript:script_name(),
    {ok, Dir} = file:get_cwd(),

    Splitted0 = filename:split(ScriptName),
    Splitted = lists:sublist(Splitted0, 1, length(Splitted0)-2),
    RootDir = case length(Splitted) of
		  0 ->
		      Dir;
		  _ ->
		      filename:join([Dir | Splitted])
	      end,

    {ok, Libs} = file:list_dir(filename:join(RootDir, "lib")),

    lists:foreach(fun(Lib) ->
			  Path = filename:join([RootDir, "lib", Lib, "ebin"]),
			  code:add_path(Path)
		  end, lists:delete(".svn", Libs)),
    
    RootDir.
    
get_info() ->
    Dir = code:root_dir(),
    Version = erlang:system_info(version),
    
    {Version, Dir}.

copy_bin_files({Version, Path}) ->
    Prefix = filename:join([Path, "erts-" ++ Version, "bin"]),
    ToCopyList = ["heart", "to_erl", "run_erl"],
    Copier = fun(X) ->
		     Source = filename:join(Prefix, X),
		     Dest = filename:join("bin", X),
		     case filelib:is_file(Dest) of
			 true ->
			     inform_exists(Dest);
			 false ->
			     case file:copy(Source, Dest) of
				 {ok, _} -> confirm_created(Dest);
				 {error, Reason} -> handle_error(Reason)
			     end,
			     file:write_file_info(Dest, #file_info{mode=8#00744})
		     end
	     end,
    lists:foreach(Copier, ToCopyList).

create_script(FileName, Content) ->
    case filelib:is_file(FileName) of
	true ->
	    inform_exists(FileName);
	false ->
	    case file:open(FileName, [write]) of
		{ok, Fd} ->
		    io:format(Fd, Content, []),
		    file:close(Fd),
		    file:write_file_info(FileName, 
					 #file_info{mode=8#00744}),
		    confirm_created(FileName);
		{error, Reason} -> handle_error(Reason)
	    end
    end.
			
create_start_scripts({_, Path}) ->
    BinStart = "#!/bin/sh\n\n"

	"ROOTDIR=`pwd`\n"
	"export TERM=xterm\n"
	"export SHELL=/bin/bash\n"
	"export HEART_COMMAND=\"$ROOTDIR/bin/start\"\n\n"

	"echo \"Starting Erlang Web\"\n"
	"RELDIR=$ROOTDIR/releases\n"
	"rm -f $ROOTDIR/erlang.log.?\n"
	"START_ERL_DATA=${1:-$RELDIR/start_erl.data}\n"
	"$ROOTDIR/bin/run_erl -daemon pipes/ $ROOTDIR/log \"exec $ROOTDIR/bin/start_erl " 
	"$ROOTDIR $RELDIR $START_ERL_DATA\"\n",
    
    BinStartName = filename:join("bin", "start"),
    create_script(BinStartName, BinStart),

    BinStop = "#!/bin/sh\n\n"

	"ROOTDIR=`pwd`\n"
	"PID=$(ps ax | grep -E .*beam.*$ROOTDIR | grep -v grep | awk '{print $1}')\n\n"

	"if [ $PID ]; then\n"
	"\techo  \"Stopping Erlang Web\"\n"
	"\tHEART_PID=$(ps ax | grep heart | grep -v beam | grep $PID | awk '{print $1}')\n"
	"\tif [ $HEART_PID ]; then\n"
	"\t\tkill $HEART_PID\n"
	"\telse\n"
	"\t\tkill $PID\n"
	"\tfi\n"
	"else\n"
	"\techo \"Erlang Web is not running\"\n"
	"fi\n",
    
    BinStopName = filename:join("bin", "stop"),
    create_script(BinStopName, BinStop),

    BinConnect = "#!/bin/sh\n\n"
	"bin/to_erl pipes/\n",
    
    BinConnectName = filename:join("bin", "connect"),
    create_script(BinConnectName, BinConnect),

    BinStartInteractive = "#!/bin/sh\n\n"

    "if [ $# -eq 0 ]
    then
        SERVER=inets
        NODE_TYPE=single_node
    elif [ $# -eq 1 ]
    then
        case $1 in
        yaws)
            SERVER=yaws
            ;;
        ewgi_mochiweb)
            SERVER=ewgi_mochiweb
            ;;
        ewgi_inets)
            SERVER=ewgi_inets
            ;;
        *)
            SERVER=inets
        esac
        NODE_TYPE=single_node
        shift
    else
        case $1 in
        yaws)
            SERVER=yaws
            ;;
        ewgi_mochiweb)
            SERVER=ewgi_mochiweb
            ;;
        ewgi_inets)
            SERVER=ewgi_inets
            ;;
        *)
            SERVER=inets
        esac
        case $2 in
        frontend)
            NODE_TYPE=frontend
            ;;
        backend)
            NODE_TYPE=backend
            ;;
        single_node_with_cache)
            NODE_TYPE=single_node_with_cache
            ;;
        *)
            NODE_TYPE=single_node
        esac
        shift
        shift
    fi

    erl -pa lib/*/ebin -s e_start start $NODE_TYPE $SERVER $@",
    
    BinStartInteractiveName = filename:join("bin", "start_interactive"),
    create_script(BinStartInteractiveName, BinStartInteractive),

    FileContent2 = "#!/bin/sh\n\n"

	"ROOTDIR=$1\n"
	"RELDIR=$2\n"
	"DataFile=$3\n\n"

	"shift\nshift\nshift\n\n"

	"ERTS_VSN=`awk '{print $2}' $DataFile`\n"
	"VSN=`awk '{print $1}' $DataFile`\n\n"

	"BINDIR=" ++ Path ++ "/erts-$ERTS_VSN/bin\n"
	"EMU=beam\n"
	"PROGNAME=`echo $0 | sed 's/.*\\///'`\n"
	"HOSTNAME=test\n\n"

	"export EMU\n"
	"export ROOTDIR\n"
	"export BINDIR\n"
	"export PROGNAME\n"
	"export RELDIR\n\n"

	"exec $BINDIR/erlexec -boot $RELDIR/$VSN/start -config $RELDIR/$VSN/sys "
	"-heart -env HEART_BEAT_TIMEOUT 30 -pa patches +K true -sname $HOSTNAME -smp auto +P 262140 ${1+\"$@\"}\n",
    
    FileName2 = filename:join("bin", "start_erl"),
    create_script(FileName2, FileContent2).

create_start_erl_data({Version, _}) ->
    Filename = filename:join("releases", "start_erl.data"),

    case filelib:is_file(Filename) of
	true ->
	    inform_exists(Filename);
	false ->
	    case file:open(Filename, [write]) of
		{ok, Fd} ->
		    io:format(Fd, "0.1 ~s", [Version]),
		    file:close(Fd),
		    confirm_created(Filename);
		{error, Reason} ->
		    handle_error(Reason)
	    end
    end.

create_basic_config_files() ->
    conf_dispatcher(),
    conf_errors(),
    conf_project(),
    conf_autocomplete().

conf_dispatcher() ->
    Filename = filename:join(["config", "dispatch.conf"]),

    case filelib:is_file(Filename) of
	true ->
	    inform_exists(Filename);
	false ->
	    case file:open(Filename, [write]) of
		{ok, Fd} ->
		    io:format(Fd, "%%~n"
			      "%% ENTRIES FOR THE AUTOCOMPLETE WPART~n"
			      "%%~n", []),
		    
		    Entries = [{static, "^/autocomplete\.css$", enoent},
			       {static, "^/jquery\.autocomplete\.js$", enoent},
			       {static, "^/jquery\.js$", enoent},
			       {static, "^/indicator\.gif$", enoent}],
		    lists:foreach(fun(Entry) ->
					  io:format(Fd, "~p.~n", [Entry])
				  end, Entries),
		    
		    io:format(Fd, "~n%%~n"
			      "%% WELCOME PAGE~n"
			      "%%~n"
			      "~p.~n", [{static, "^/?$", "welcome.html"}]),
		    
		    file:close(Fd),
		    confirm_created(Filename);
		{error, Reason} ->
		    handle_error(Reason)
	    end
    end.

conf_errors() ->
    Filename = filename:join(["config", "errors.conf"]),

    case filelib:is_file(Filename) of
	true ->
	    inform_exists(Filename);
	false ->
	    case file:open(Filename, [write]) of
		{ok, Fd} ->
		    io:format(Fd, "{error, 501, \"templates/501.html\"}.\n"
			      "{error, 404, \"templates/404.html\"}.\n", []),
		    file:close(Fd),
		    confirm_created(Filename),
		    
		    ErrorPage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" "
			"\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
			"<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
			"<head>\n"
			"<title>~p Error</title>\n"
			"</head>\n"
			"<body>\n"
			"<center>\n"
			"<h1>~p Error</h1>\n"
			"</center>\n"
			"</body>\n"
			"</html>",

		    Filename404 = filename:join("templates", "404.html"),
		    case filelib:is_file(Filename404) of
			true ->
			    inform_exists(Filename404);
			false ->
			    case file:open(Filename404, [write]) of
				{ok, Fd404} ->
				    io:format(Fd404, ErrorPage, [404, 404]),
				    file:close(Fd404),
				    confirm_created(Filename404);
				{error, Reason1} ->
				    handle_error(Reason1)
			    end
		    end,
		    
		    Filename501 = filename:join(["templates", "501.html"]),
		    case filelib:is_file(Filename501) of
			true ->
			    inform_exists(Filename501);
			false ->
			    case file:open(Filename501, [write]) of
				{ok, Fd501} ->
				    io:format(Fd501, ErrorPage, [501, 501]),
				    file:close(Fd501),
				    confirm_created(Filename501);
				{error, Reason2} ->
				    handle_error(Reason2)
			    end;
			{error, Reason} ->
			    handle_error(Reason)
		    end
	    end
    end.

conf_project() ->
    Filename = filename:join(["config", "project.conf"]),

    case filelib:is_file(Filename) of
	true ->
	    inform_exists(Filename);
	false ->
	    case file:open(Filename, [write]) of
		{ok, Fd} ->
		    io:format(Fd, "{http_port, 8080}.~n", []),
		    file:close(Fd),
		    confirm_created(Filename);
		{error, Reason} ->
		    handle_error(Reason)
	    end
    end.

conf_autocomplete() ->
    Wparts = code:priv_dir(wparts),
    lists:foreach(fun(File) ->
			  Dest = filename:join(["docroot", File]),
			  case filelib:is_file(Dest) of
			      true ->
				  inform_exists(Dest);
			      false ->
				  case file:copy(filename:join([Wparts, File]), Dest) of
				      {ok, _} ->
					  confirm_created(Dest);
				      {error, Reason} ->
					  handle_error({Dest, Reason})
				  end
			  end
		  end, ["autocomplete.css", "indicator.gif", "jquery.autocomplete.js", 
			"jquery.js"]).

create_welcome_page() ->
    Filename = filename:join(["templates", "welcome.html"]),

    case filelib:is_file(Filename) of
	true ->
	    inform_exists(Filename);
	false ->
	    case file:open(Filename, [write]) of
		{ok, Fd} ->
		    Content = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
			"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n"
			"\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
			"<html xmlns=\"http://www.w3.org/1999/xhtml\">\n\n"
	    
			"<head>\n"
			"<title>Erlang Web</title>\n"
			"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n"
			"</head>\n"
			"<body>\n"
			"<h1>Welcome to the Erlang Web!</h1>\n"
			"</body>\n"
			"</html>\n",
		    io:format(Fd, Content, []),
		    file:close(Fd),
		    confirm_created(Filename);
		{error, Reason} ->
		    handle_error(Reason)
	    end
    end.

create_rel_file({Version, _}, Server) ->
    Name = "start",
    Filename = filename:join(["releases", "0.1", Name ++ ".rel"]),
    
    case filelib:is_file(Filename) of
	true ->
	    inform_exists(Filename);
	false ->
	    case file:open(Filename, [write]) of
		{ok, Fd} ->
		    Apps = get_apps_for_release(Server),
		    ReleaseInfo = {release, {Name, "0.1"}, {erts, Version},
				   Apps},
		    
		    io:format(Fd, "~p.~n", [ReleaseInfo]),
		    file:close(Fd),
		    confirm_created(Filename);
		{error, Reason} ->
		    handle_error(Reason)
	    end
    end.

get_apps_for_release(ewgi_inets) ->
    ToLoad = get_apps_for_release(inets),
    application:load(ewgi),
    {value, {_, _, Ver}} = lists:keysearch(ewgi, 1, application:loaded_applications()),
    [{ewgi, Ver} | ToLoad];
get_apps_for_release(Server) ->
    {ok, Dir} = file:list_dir("lib/"),
    [code:add_path("lib/" ++ D ++ "/ebin") || D <- Dir],

    ToLoad = [xmerl, sasl, crypto, eptic, wpart, wparts, mnesia, ssl,
	      syntax_tools, compiler, runtime_tools, Server],
    [application:load(App) || App <- ToLoad],

    lists:map(fun({Name, _, Vsn}) ->
		      {Name, Vsn}
	      end, application:loaded_applications()).

generate_boot_file() ->
    systools:make_script("start", [{path, ["releases/0.1", "lib/*/ebin"]}, 
				   {outdir, "releases/0.1/"}, silent]),
    confirm_created("releases/0.1/start.script"),
    confirm_created("releases/0.1/start.boot"),
    
    file:copy("releases/0.1/start.rel", "start.rel"),
    systools:make_tar("start",[{path, ["releases/0.1"]}, {outdir, "releases/0.1/"}, 
			       {dirs, [include, src]}, silent]),
    file:delete("start.rel"),
    
    erl_tar:extract("releases/0.1/start.tar.gz", [keep_old_files, compressed]).

copy_conf_files() ->
    YawsConfig = "config/yaws.conf",
    case filelib:is_file(YawsConfig) of
	true ->
	    inform_exists(YawsConfig);
	false ->
	    file:copy(code:priv_dir(eptic) ++ "/yaws.conf", YawsConfig),
	    confirm_created(YawsConfig)
    end,

    MimeTypes = "docroot/conf/mime.types",
    case filelib:is_file(MimeTypes) of
	true ->
	    inform_exists(MimeTypes);
	false ->
	    file:copy(code:priv_dir(eptic) ++ "/mime.types", MimeTypes),
	    confirm_created(MimeTypes)
    end,

    InetsConfig = "config/inets.conf",
    case filelib:is_file(InetsConfig) of
	true ->
	    inform_exists(InetsConfig);
	false ->
	    file:copy(code:priv_dir(eptic) ++ "/inets.conf", InetsConfig),
	    confirm_created(InetsConfig)
    end,

    InetsHttpsConfig = "config/inets_https.conf",
    case filelib:is_file(InetsHttpsConfig) of
	true ->
	    inform_exists(InetsHttpsConfig);
	false ->
	    file:copy(code:priv_dir(eptic) ++ "/inets_https.conf", InetsHttpsConfig),
	    confirm_created(InetsHttpsConfig)
    end,

    EwgiInetsConfig = "config/ewgi_inets.conf",
    case filelib:is_file(EwgiInetsConfig) of
	true ->
	    inform_exists(EwgiInetsConfig);
	false ->
	    file:copy(code:priv_dir(eptic) ++ "/ewgi_inets.conf", EwgiInetsConfig),
	    confirm_created(EwgiInetsConfig)
    end,

    EwgiInetsHttpsConfig = "config/ewgi_inets_https.conf",
    case filelib:is_file(EwgiInetsHttpsConfig) of
	true ->
	    inform_exists(EwgiInetsHttpsConfig);
	false ->
	    file:copy(code:priv_dir(eptic) ++ "/ewgi_inets_https.conf", EwgiInetsHttpsConfig),
	    confirm_created(EwgiInetsHttpsConfig)
    end,
    
    ErrorsConfig = "config/errors_description.conf",
    case filelib:is_file(ErrorsConfig) of
	true ->
	    inform_exists(ErrorsConfig);
	false ->
	    file:copy(code:priv_dir(eptic) ++ "/errors.conf", ErrorsConfig),
	    confirm_created(ErrorsConfig)
    end.

create_sys_config_file(yaws) ->
    Filename = "releases/0.1/sys.config",
    case filelib:is_file(Filename) of
	true ->
	    inform_exists(Filename);
	false ->
	    case file:open(Filename, [write]) of
		{ok, Fd} ->
		    Content = [{yaws, [{conf, "config/yaws.conf"}]}],
		    io:format(Fd, "~p.~n", [Content]),
		    confirm_created(Filename),
		    file:close(Fd);
		{error, Reason} ->
		    handle_error(Reason)
	    end
    end;
create_sys_config_file(inets) ->
    Filename = "releases/0.1/sys.config",
    case filelib:is_file(Filename) of
	true ->
	    inform_exists(Filename);
	false ->
	    case file:open(Filename, [write]) of
		{ok, Fd} ->
		    Content = [{inets, [{services, [{httpd, "config/inets.conf"}]}]}],
		    io:format(Fd, "~p.~n", [Content]),
		    confirm_created(Filename),
		    file:close(Fd);
		{error, Reason} ->
		    handle_error(Reason)
	    end
    end;
create_sys_config_file(ewgi_inets) ->
    Filename = "releases/0.1/sys.config",
    case filelib:is_file(Filename) of
	true ->
	    inform_exists(Filename);
	false ->
	    case file:open(Filename, [write]) of
		{ok, Fd} ->
		    Content = [{inets, [{services, [{httpd, "config/ewgi_inets.conf"}]}]},
                {ewgi, [{app_module, e_mod_ewgi}, {app_function, do}]}],
		    io:format(Fd, "~p.~n", [Content]),
		    confirm_created(Filename),
		    file:close(Fd);
		{error, Reason} ->
		    handle_error(Reason)
	    end
    end.

copy_escripts(RootDir) ->
    case file:get_cwd() of
	{ok, RootDir} ->
	    ok;
	{ok, _} ->
	    Files = [filename:join(["bin", "compile.erl"]),
		     filename:join(["bin", "add.erl"]),
		     filename:join(["bin", "generate.erl"]),
		     filename:join(["bin", "test.erl"]),
		     filename:join(["bin", "e_component.erl"])],
	    
	    case filelib:is_file("Emakefile") of
		true ->
		    inform_exists("Emakefile");
		false ->
		    file:copy(filename:join(RootDir, "Emakefile"), "Emakefile"),
		    confirm_created("Emakefile")
	    end,
	    
	    lists:foreach(fun(File) ->
				  case filelib:is_file(File) of
				      true ->
					  inform_exists(File);
				      false ->
					  file:copy(filename:join([RootDir, File]), File),
					  file:write_file_info(File, 
							       #file_info{mode=8#00744}),
					  confirm_created(File)
				  end
			  end, Files)
    end.
 
