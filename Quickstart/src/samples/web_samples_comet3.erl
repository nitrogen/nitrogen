-module (web_samples_comet3).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.
	
title() -> "Multiple Comets with Graceful Exit".
headline() -> "Multiple Comets with Graceful Exit".
right() -> linecount:render().

body() -> 
	Body = [
		#span { text="Cycle through colors: " },
		#span { id=colorLabel, text="-" },
		#p{},
		#span { text="Cycle through directions: " },
		#span { id=directionLabel, text="-" }
  ],

	% Start the counter as a background process.
	wf:comet(fun() -> cycle_and_update(1000, colorLabel, ["Red", "Orange", "Yellow", "Green", "Blue", "Indigo", "Violet"]) end),
	wf:comet(fun() -> cycle_and_update(2000, directionLabel, ["North", "East", "South", "West"]) end),

	wf:render(Body).

event(_) -> ok.

cycle_and_update(Speed, ControlID, List) ->
	% Don't exit on error...
	process_flag(trap_exit, true),
	
	% Check to see if we have received an EXIT notice.
	receive 
		{'EXIT', _, stop_comet} -> 
			io:format("The user has left the page.~n"),
			exit(done)
	after 0 -> continue
	end,

	% Sleep for a second, then update
	timer:sleep(Speed),
	
	% Update the control.
	wf:update(ControlID, hd(List)),
	
	% wf:comet_flush() is only needed because we are looping. Otherwise,
	% we could just let the function complete.
	wf:comet_flush(),
	
	% Take the first item from the list, make it the last item on the list.
	% So if we start with [1, 2, 3, 4], we'd end with [2, 3, 4, 1]
	List1 = lists:reverse([hd(List) | lists:reverse(tl(List))]),

	% Loop. This process will automatically be killed
	% once the page stops requesting the output that
	% it generates.
	cycle_and_update(Speed, ControlID, List1).
	
