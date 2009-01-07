% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_min_length).
-include ("wf.inc").
-compile(export_all).

render_validator(TriggerPath, TargetPath, Record)  ->
	Text = wf_utils:js_escape(Record#min_length.text),
	Length = Record#min_length.length,
	validator_custom:render_validator(TriggerPath, TargetPath, #custom { function=fun validate/2, text = Text, tag=Record }),
	wf:f("v.add(Validate.Length, { minimum: ~p, tooShortMessage: \"~s\" });", [Length, Text]).

validate(Record, Value) ->
	Record#min_length.length =< length(Value).
