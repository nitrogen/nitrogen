-module (validator_is_integer).
-include ("wf.inc").
-compile(export_all).

render_validator(TriggerPath, TargetPath, Record) -> 
	Text = wf_utils:js_escape(Record#is_integer.text),
	validator_custom:render_validator(TriggerPath, TargetPath, #custom { function=fun validate/2, text = Text, record=Record }),
	wf:f("v.add(Validate.Numericality, { notAnIntegerMessage: \"~s\", onlyInteger: true });", [Text]).

validate(Value, _) -> 
	try _X = list_to_integer(Value), true
	catch _ : _ -> false
	end.
