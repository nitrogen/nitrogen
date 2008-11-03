-module (wf_validation).
-include ("wf.inc").
-export ([validate/1]).

validate(TriggerPath) ->
	Validators = [X || X <- wf:state(validators), element(1, X) == TriggerPath],
	F = fun({_, TargetPath, Record}, FailedPaths) ->
		case lists:member(TargetPath, FailedPaths) of
			true -> FailedPaths;
			false ->
				Function = Record#custom.function,
				Text = Record#custom.text,
				[Value] = wf:q(wf_path:to_path(TargetPath)),
				case Function(Value, Record#custom.record) of
					true -> FailedPaths;
					false ->
						wf:wire(TargetPath, #validation_error { text=Text }),
						FailedPaths ++ [TargetPath]
				end
		end
	end,
 	lists:foldl(F, [], Validators) == [].
