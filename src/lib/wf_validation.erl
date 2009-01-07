% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_validation).
-include ("wf.inc").
-export ([validate/1]).

validate(TriggerPath) ->
	TriggerPath1 = wf_path:to_path(TriggerPath),
	
	% Get all validators that match the trigger path.
	% TriggerPath is the full path of the Nitrogen element.
	% The Validator's path will only have a partial path.
	% ie: TriggerPath      = [nameTextBox, div1, page]
	%     Validator's path = [nameTextBox, div1]
	F1 = fun({P, _, _}) ->
		P1 = wf_path:to_path(P),
		case length(TriggerPath1) >= length(P1) of
			true -> 
				{Pre, _} = lists:split(length(P1), TriggerPath1),
				Pre == P1;
			false -> 
			  false
		end
	end,	
	Validators = lists:filter(F1, wf:state(validators)),
	
	
	% Now, run through each matching validator.
	% Stop validating a TargetPath when it has failed.
	F2 = fun({_, TargetPath, Record}, FailedPaths) ->
		case lists:member(TargetPath, FailedPaths) of
			true -> FailedPaths;
			false ->
				Function = Record#custom.function,
				Text = Record#custom.text,
				[Value] = wf:q(wf_path:to_path(TargetPath)),
				
				case Function(Record#custom.tag, Value) of
					true -> FailedPaths;
					false ->
						wf:wire(TargetPath, #validation_error { text=Text }),
						FailedPaths ++ [TargetPath]
				end
		end
	end,
 	lists:foldl(F2, [], Validators) == [].

