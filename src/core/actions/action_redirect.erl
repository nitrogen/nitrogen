% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_redirect).
-include ("wf.inc").
-compile(export_all).

render_action(Record, Context) ->
	DestinationUrl = Record#redirect.url,
	Script = wff:f("window.location=\"~s\";", [wf_utils:js_escape(DestinationUrl)]),
	{ok, Script, Context}.
	
redirect(Url, Context) -> 
	wff:wire(#redirect { url=Url }, Context).
