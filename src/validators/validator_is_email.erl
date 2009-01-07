% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_is_email).
-include ("wf.inc").
-compile(export_all).

render_validator(TriggerPath, TargetPath, Record)  ->
	Text = wf_utils:js_escape(Record#is_email.text),
	validator_custom:render_validator(TriggerPath, TargetPath, #custom { function=fun validate/2, text = Text, tag=Record }),
	wf:f("v.add(Validate.Email, { failureMessage: \"~s\" });", [Text]).

validate(_, Value) ->
	Result = regexp:match(wf:to_list(Value), "([^@\s]+)@((?:[-a-z0-9]+\.)+[a-z]{2,})"),
	Result /= nomatch.
