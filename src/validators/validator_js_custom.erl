% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_js_custom).
-include ("wf.inc").
-compile(export_all).

render_validator(_TriggerPath, _TargetPath, Record) -> 
	Text = wf_utils:js_escape(Record#js_custom.text),
	Function = Record#js_custom.function,
	Args = Record#js_custom.args,
	wf:f("v.add(Validate.Custom, { against: ~s, args: ~s, failureMessage: \"~s\" });", [Function, Args, Text]).