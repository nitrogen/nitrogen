% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_alert).
-include ("wf.inc").
-compile(export_all).

render_action(Record, Context) -> 
	Script = wf:f("alert(\"~s\");", [wf_utils:js_escape(Record#alert.text)]),
	{ok, Script, Context}.
	