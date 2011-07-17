-module(media).
-export([streamlist/1, streamlist_json/1]).

streamlist(_Args) ->
    List = rpc:call(config:get(erlyvideo_node), media_provider, entries, [default]),
    wpart:fset("media", [ [{"name", binary_to_list(Name)}] || {_Type, Name, _Pid} <- List]),
    {template, "media/streamlist.html"}.


streamlist_json(_Args) ->
    %console:log('STREAMLIST JSON'),
    ListJ = [ {struct, [{"name", Name}]} || {_Type, Name, _Pid} <-
	rpc:call(config:get(erlyvideo_node), media_provider, entries, [default])],
    %console:log(["STREAMLIST JSON LIST", ListJ]),
    case ListJ of
	[] -> {content, text, "[]"};
	_  -> {content, text, jsonutils:encode(ListJ)}
    end.