% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_validate).
-include ("wf.inc").
-compile(export_all).

render_action(Record, Context) -> 
	% Some values...
	TriggerPath = Record#validate.trigger,
	TargetPath = Record#validate.target,
	ValidMessage = wf_utils:js_escape(Record#validate.success_text),
	OnlyOnBlur = (Record#validate.on == blur),
	OnlyOnSubmit = (Record#validate.on == submit),	
	InsertAfterNode = case Record#validate.attach_to of
		undefined -> "";
		Node -> wff:f(", insertAfterWhatNode : obj(\"~s\")", [Node])
	end,

	% Create the validator Javascript...
	ConstructorJS = wff:f("var v = obj('me').validator = new LiveValidation(obj('me'), { validMessage: \"~s\", onlyOnBlur: ~s, onlyOnSubmit: ~s ~s});", [wf_utils:js_escape(ValidMessage), OnlyOnBlur, OnlyOnSubmit, InsertAfterNode]),
	TriggerJS = wff:f("v.trigger = obj('~s');", [wff:to_js_id(TriggerPath)]),
	
	% Update all child validators with TriggerPath and TargetPath...
	F = fun(X) ->
			Base = wf_utils:get_validatorbase(X),
			Base1 = Base#validatorbase { trigger = TriggerPath, target = TargetPath },
			wf_utils:replace_with_base(Base1, X)
	end,
	Validators = lists:flatten([Record#validate.validators]),
	Validators1 = [F(X) || X <- Validators],

  % Use #script element to create the final javascript to send to the browser...
	Actions = [
		ConstructorJS, TriggerJS, Validators1
	],
	{ok, Actions, Context}.
	

