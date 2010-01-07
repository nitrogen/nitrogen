% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_validation).
-include ("wf.inc").
-export ([validate/0]).

validate() ->
	% Some values...
	ValidationGroup = wf_context:event_validation_group(),
	Validators = state_handler:get_state(validators, []),
	
	?PRINT(ValidationGroup),
	?PRINT(Validators),

	% Get all validators that match the trigger path.
	% TriggerPath is the full path of the Nitrogen element.
	% The Validator's path will only have a partial path.
	% ie: TriggerPath      = [nameTextBox, div1, page]
	%     Validator's path = [nameTextBox, div1]
	% F1 = fun({P, _, _}) ->
	% 	case length(TriggerPath) >= length(P) of
	% 		true -> 
	% 			{Pre, _} = lists:split(length(P), TriggerPath),
	% 			Pre == P;
	% 		false -> 
	% 		  false
	% 	end
	% end,
	% Validators1 = lists:filter(F1, Validators),
	% 
	% 
	% % Now, run through each matching validator.
	% % Stop validating a TargetPath when it has failed.
	% F2 = fun({_, TargetPath, Record}, FailedPaths) ->
	% 	case lists:member(TargetPath, FailedPaths) of
	% 		true -> 
	% 			FailedPaths;
	% 		false ->
	% 			Function = Record#custom.function,
	% 			Text = Record#custom.text,
	% 			HtmlID = wf:to_html_id(TargetPath),
	% 			Value = wf:q(HtmlID),
	% 			
	% 			case Function(Record#custom.tag, Value) of
	% 				true -> 
	% 					FailedPaths;
	% 				false ->
	% 					wf:wire(TargetPath, #validation_error { text=Text }),
	% 					FailedPaths ++ [TargetPath]
	% 			end
	% 	end
	% end,
	% 
	% FailedPaths1 = lists:foldl(F2, [], Validators1),
	% {ok, FailedPaths1 == []}.
	{ok, true}.
