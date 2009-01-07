% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_alert).
-include ("wf.inc").
-compile(export_all).

render_action(_TriggerPath, _TargetPath, Record) -> 
	wf:f("alert(\"~s\");", [wf_utils:js_escape(Record#alert.text)]).
	