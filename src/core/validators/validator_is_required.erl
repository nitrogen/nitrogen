% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_is_required).
-include ("wf.inc").
-compile(export_all).

render_action(Record, Context) -> 
	TriggerPath = Record#is_required.trigger,
	TargetPath = Record#is_required.target,
	Text = wf_utils:js_escape(Record#is_required.text),
	CustomValidatorAction = #custom { trigger=TriggerPath, target=TargetPath, function=fun validate/2, text=Text, tag=Record },
	Script = [], % wff:f("obj('me').validator.add(Validate.Presence, { failureMessage: \"~s\" });", [Text]),
	{ok, [CustomValidatorAction, Script], Context}.

validate(_, Value) -> 
	Value /= [].
