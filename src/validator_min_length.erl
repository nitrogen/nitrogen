-module (validator_min_length).
-include ("wf.inc").
-compile(export_all).

render_validator(TriggerPath, TargetPath, Record)  ->
	Text = wf_utils:js_escape(Record#min_length.text),
	Length = Record#min_length.length,
	validator_custom:render_validator(TriggerPath, TargetPath, #custom { function=fun validate/2, text = Text, record=Record }),
	wf:f("v.add(Validate.Length, { minimum: ~p, tooShortMessage: \"~s\" });", [Length, Text]).

validate(Value, Record) ->
	Record#min_length.length =< length(Value).
