% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_validation_error).
-include ("wf.inc").
-compile(export_all).

render_action(Record, Context) -> 
	TriggerPath = Record#validation_error.trigger,
	TargetPath = Record#validation_error.target,
	Text = wf_utils:js_escape(Record#validation_error.text),
	Script = [
		"var v = new LiveValidation(obj('me'), { onlyOnSubmit: true });",
		wff:f("v.add(Validate.Custom, { against: Nitrogen.$return_false, failureMessage: \"~s\", displayMessageWhenEmpty: true });", [Text]),
		"v.validate();"
	],
	
	Action = #script { trigger=TriggerPath, target=TargetPath, script=Script },
	{ok, Action, Context}.
		
	
