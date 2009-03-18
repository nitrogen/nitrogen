% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% Contributions from Torbjorn Tornkvist (tobbe@tornkvist.org)
% See MIT-LICENSE for licensing information.

-module (validator_max_length).
-include ("wf.inc").
-compile(export_all).

render_validator(TriggerPath, TargetPath, Record)  ->
	Text = wf_utils:js_escape(Record#max_length.text),
	Length = Record#max_length.length,
	validator_custom:render_validator(TriggerPath, TargetPath, #custom { function=fun validate/2, text = Text, tag=Record }),
	wf:f("v.add(Validate.Length, { maximum: ~p, tooLongMessage: \"~s\" });", [Length, Text]).

validate(Record, Value) ->
	Record#max_length.length >= length(Value).
