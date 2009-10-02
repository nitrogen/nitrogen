-module (web_nbe_5_1).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	wf:state(current_count, 0),
	
	[
		#button { text="Count", postback=count },
		#p {},
		#span { id=myspan }
	].
	
event(count) ->
	% Get the old count...
	CurrentCount = wf:state(current_count),
	
	% Update with new count...
	NewCount = CurrentCount + 1,
	wf:state(current_count, NewCount),
	
	% Display new message...
	S = wf:f("The current count is ~p.", [NewCount]),
	wf:update(myspan, S).
