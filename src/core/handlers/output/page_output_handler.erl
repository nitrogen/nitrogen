% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (page_output_handler).
-behaviour (output_handler).
-include ("wf.inc").
-include ("simplebridge.hrl").
-export ([
	init/1, 
	finish/2,
	build_response/4
]).

init(Context) -> 
	{ok, Context, []}.
	
finish(Context, State) -> 
	{ok, Context, State}.

build_response(Html, Script, Context, _State) ->
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
	"</script>\n",
	"</html>"
	],
	
	?PRINT(SimpleBody),
	SimpleBody1 = replace(body, Html, SimpleBody),
	?PRINT(SimpleBody1),
	SimpleBody2 = replace(script, Script, SimpleBody1),
	?PRINT(SimpleBody2),
	% Update the response bridge and return.
	Bridge = Context#context.response,
	Bridge1 = Bridge:data(lists:flatten(SimpleBody2)),
	Bridge1:build_response().
	
replace(Old, New, [Old|T]) -> [New|T];
replace(Old, New, [H|T]) -> [H|replace(Old, New, T)];
replace(_, _, []) -> [].