-module (web_nbe_5_2).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	% Set initial count...
	case wf:session(current_count) of
		undefined -> wf:session(current_count, 0);
		_ -> ignore
	end,
	
	% Create controls...
	[
		#button { text="Count", postback=count },
		#p {},
		#span { id=myspan }
	].
	
event(count) ->
	% Get the old count...
	CurrentCount = wf:session(current_count),
	
	% Update with new count...
	NewCount = CurrentCount + 1,
	wf:session(current_count, NewCount),
	
	% Display new message...
	S = wf:f("The current count is ~p.", [NewCount]),
	wf:update(myspan, S).
