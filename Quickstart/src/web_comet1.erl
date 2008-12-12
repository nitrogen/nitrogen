-module (web_comet1).
-include ("wf.inc").
-export ([main/0, event/1]).

main() ->
	Title = "Simple Comet Example",
	Body = #template { file=onecolumn, title=Title, headline=Title,
	section1=[
		#span { text="Counter updated via Comet: " },
		#span { id=myCounter, text="-" }
	]},

	% Start the comet process...
	wf:comet(fun() -> background_update(myCounter, 1) end),

	wf:render(Body).
	
event(_) -> ok.


background_update(ControlID, Count) ->
	% Sleep for a second, then update
	timer:sleep(1000),
	
	% Update the control.
	wf:update(ControlID, wf:to_list(Count)),

	% wf:comet_flush() is only needed because we are looping. Otherwise,
	% we could just let the function complete.
	wf:comet_flush(),

	% Loop. This process will automatically be killed
	% once the page stops requesting the output that
	% it generates.
	background_update(ControlID, Count + 1).
	
