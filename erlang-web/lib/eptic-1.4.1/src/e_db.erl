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
%% Ltd. Portions created by Erlang Training & Consulting Ltd are Copyright 2008,
%% Erlang Training & Consulting Ltd. All Rights Reserved.

%%% File    : e_db.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang.consulting.com>
%%% @doc A common interface to the all supported database engines.
%%% Its behaviour depends on the database engine framework uses.
%%% The engine could be set inside the <i>project.conf</i> file.
%%% @end
%%% @see e_conf:db/0
%%% @see e_db_couchdb
%%% @see e_db_mnesia
-module(e_db).

-export([install/0]).
-export([start/0]).
-export([read/1, read/2, delete/2, write/2, update/2, size/1, get_next_id/1]).

%%
%% @spec install() -> any()
%% @doc Sets up the environment for the selected database.
%%
-spec(install/0 :: () -> any()).
install() ->
    Mod = e_conf:dbms(),
    Mod:install().

%%
%% @spec start() -> ok | {error, Reason :: term()}
%% @doc Starts the DBMS.
%%
-spec(start/0 :: () -> ok | {error, term()}).
start() ->	     
    Mod = e_conf:dbms(),
    Mod:start().

%%
%% @spec read(Type :: atom()) -> [Element] | {error, Reason}
%%   Element = tuple()
%% @doc Reads and returns all the entites stored in the database of the given <i>Type</i>.<br/>
%% In case of error, <i>{error, Reason}</i> is returned.
%% @end
%%
-spec(read/1 :: (atom()) -> list(tuple())).
read(Type) ->
    Mod = e_conf:dbms(),
    Mod:read(Type).

%% 
%% @spec read(Type :: atom(), Id :: any()) -> Element :: tuple() | not_found | {error, Reason}
%% @doc Reads and returns the entity of the given <i>Type</i> and <i>Id</i>.
%% The pair <i>{Type, Id}</i> should match at most one element stored in the database. <br/>
%% When the element is not found, the atom <i>not_found</i> is returned.<br/>
%% In case of error, the <i>{error, Reason}</i> is returned.
%% @end
%%
-spec(read/2 :: (atom(), any()) -> tuple() | not_found | {error, any()}).
read(Type, Id) ->
    Mod = e_conf:dbms(),
    Mod:read(Type, Id).

%%
%% @spec delete(Type :: atom(), Key :: any()) -> ok | {error, Reason}
%% @doc Removes the given element of the <i>Type</i> with <i>Key</i> from the database.
%% If removal process succeeds, <i>ok</i> is returned.<br/>
%% In case of error, the <i>{error, Reason}</i> is returned.
%% @end
%%
-spec(delete/2 :: (atom(), any()) -> ok | {error, any()}).	     
delete(Type, Key) ->
    Mod = e_conf:dbms(),
    Mod:delete(Type, Key).

%%
%% @spec write(Type :: atom(), Element :: tuple()) -> ok | {error, Reason}
%% @doc Writes the <i>Element</i> of the <i>Type</i> to the database.
%% If writing process succeeds, <i>ok</i> is returned.<br/>
%% In case of error, the <i>{error, Reason}</i> is returned.
%% @end
%%
-spec(write/2 :: (atom(), tuple()) -> ok | {error, any()}).	     
write(Type, Element) ->
    Mod = e_conf:dbms(),
    Mod:write(Type, Element).

%%
%% @spec update(Type :: atom(), Element :: tuple()) -> ok | {error, Reason}
%% @doc Updates the <i>Element</i> of the <i>Type</i> in the database.
%% If update succeeds, <i>ok</i> us returned.<br/>
%% In case of error, the <i>{error, Reason}</i> is returned.
%% @end
%%
-spec(update/2 :: (atom(), tuple()) -> ok | {error, any()}).	     
update(Type, Element) ->
    Mod = e_conf:dbms(),
    Mod:update(Type, Element).

%%
%% @spec size(Type :: atom()) -> integer() | {error, Reason}
%% @doc Checks the number of elements of the <i>Type</i> stored in database.
%%
-spec(size/1 :: (atom()) -> integer() | {error, any()}).	    
size(Type) ->
    Mod = e_conf:dbms(),
    Mod:size(Type).

%%
%% @spec get_next_id(Type :: atom()) -> integer() | {error, Reason}
%% @doc Generates and returns the unique number for the <i>Type</i>.
%% The operation is transactional. The generated id could be used for
%% creating more sophisticated id's (it could be for example combined
%% with the <i>Type</i> - but there is no need to).<br/>
%% This function will never return the same value twice for the same <i>Type</i>
%% @end
%%
-spec(get_next_id/1 :: (atom()) -> integer() | {error, any()}).	     
get_next_id(Type) ->
    Mod = e_conf:dbms(),
    Mod:get_next_id(Type).
