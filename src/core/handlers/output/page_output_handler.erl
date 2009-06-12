% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (page_output_handler).
-behaviour (output_handler).
-include ("wf.inc").
-include ("simplebridge.hrl").
-export ([
	init/2, 
	finish/2,
	build_response/4
]).

init(Context, State) -> 
	{ok, Context, State}.
	
finish(Context, State) -> 
	{ok, Context, State}.

build_response(Html, Script, Context, State) ->
	Event = Context#context.event_context,
	case Event#event_context.is_first_request of
		true -> build_first_response(Html, Script, Context, State);
		_    -> build_postback_response(Html, Script, Context, State)
	end.
	
build_first_response(Html, Script, Context, _State) ->
	SimpleBody = [
	"<html>\n",
	"<head>\n",
	"<script src='/nitrogen/jquery.js' type='text/javascript' charset='utf-8'></script>\n",
	"<script src='/nitrogen/jquery-ui.js' type='text/javascript' charset='utf-8'></script>\n",
	"<script src='/nitrogen/livevalidation.js' type='text/javascript' charset='utf-8'></script>\n",
	"<script src='/nitrogen/nitrogen.js' type='text/javascript' charset='utf-8'></script>\n",
	"<link rel='stylesheet' href='/nitrogen/jquery-ui/jquery-ui.css' type='text/css' media='screen' charset='utf-8'>\n",
	"<link rel='stylesheet' href='/nitrogen/elements.css' type='text/css' media='screen' charset='utf-8'>\n",
	"</head>\n",
	"<body>\n",
	body,
	"<script>\n",
	script,
	"\n</script>\n",
	"</html>"
	],
	SimpleBody1 = replace(body, Html, SimpleBody),
	SimpleBody2 = replace(script, Script, SimpleBody1),

	% Update the response bridge and return.
	Bridge = Context#context.response,
	Bridge1 = Bridge:data(lists:flatten(SimpleBody2)),
	Bridge1:build_response().
	
build_postback_response(_Html, Script, Context, _State) ->
	% Update the response bridge and return.
	Bridge = Context#context.response,
	Bridge1 = Bridge:data(lists:flatten(Script)),
	Bridge1:build_response().
	
	
replace(Old, New, [Old|T]) -> [New|T];
replace(Old, New, [H|T]) -> [H|replace(Old, New, T)];
replace(_, _, []) -> [].