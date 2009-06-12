% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_validation).
-include ("wf.inc").
-export ([validate/1]).

validate(Context) ->
	% Some values...
	Event = Context#context.event_context,
	TriggerPath = Event#event_context.trigger,
	Validators = state_handler:get_state(validators, [], Context),

	% Get all validators that match the trigger path.
	% TriggerPath is the full path of the Nitrogen element.
	% The Validator's path will only have a partial path.
	% ie: TriggerPath      = [nameTextBox, div1, page]
	%     Validator's path = [nameTextBox, div1]
	F1 = fun({P, _, _}) ->
		case length(TriggerPath) >= length(P) of
			true -> 
				{Pre, _} = lists:split(length(P), TriggerPath),
				Pre == P;
			false -> 
			  false
		end
	end,
	Validators1 = lists:filter(F1, Validators),

	
	% Now, run through each matching validator.
	% Stop validating a TargetPath when it has failed.
	F2 = fun({_, TargetPath, Record}, {FailedPaths, Cx}) ->
		case lists:member(TargetPath, FailedPaths) of
			true -> FailedPaths;
			false ->
				Function = Record#custom.function,
				Text = Record#custom.text,
				HtmlID = wff:to_html_id(TargetPath),
				Value = wff:q(HtmlID, Context),
				
				case Function(Record#custom.tag, Value) of
					true -> 
						{FailedPaths, Cx};
					false ->
						{ok, Cx1} = wff:wire(TargetPath, #validation_error { text=Text }, Cx),
						{FailedPaths ++ [TargetPath], Cx1}
				end
		end
	end,
	{FailedPaths, Context1} = lists:foldl(F2, {[], Context}, Validators1),
	{ok, FailedPaths == [], Context1}.
