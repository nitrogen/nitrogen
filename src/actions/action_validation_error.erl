% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_validation_error).
-include ("wf.inc").
-compile(export_all).

render_action(Record) -> 
	TargetPath = Record#validation_error.target,
	Text = wf:js_escape(Record#validation_error.text),
	[
		wf:f("var v = new LiveValidation(obj('~s'), { onlyOnSubmit: true });", [TargetPath]),
		wf:f("v.add(Validate.Custom, { against: Nitrogen.$return_false, failureMessage: \"~s\", displayMessageWhenEmpty: true });", [Text]),
		"v.validate();"
	].
		
	
