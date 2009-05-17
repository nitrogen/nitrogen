% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_confirm_password).
-include ("wf.inc").
-compile(export_all).

render_validator(TriggerPath, TargetPath, Record)  ->
	Text = wf_utils:js_escape(Record#confirm_password.text),
	PasswordElement = wf:to_js_id(Record#confirm_password.password),

	validator_custom:render_validator(TriggerPath, TargetPath, #custom { function=fun validate/2, text = Text, tag=Record }),

	JSFunction = wf:f("function(value, args) { return (value == obj('~s').value); }", [PasswordElement]),
	validator_js_custom:render_validator(TriggerPath, TargetPath, #js_custom { function=JSFunction, text=Text }).

validate(Record, Value) ->
	[Password] = wf:q(Record#confirm_password.password),
	Value == Password.
