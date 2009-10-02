-module (web_nbe_1_6).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	wf:wire(mybutton, #event { type=mouseover, postback=moused_over }),
	wf:wire(mybutton, #event { type=click, postback=clicked }),
	wf:wire(mybutton, #event { type=mouseout, postback=moused_out }),

	[
		#button { id=mybutton, text="Button" },
		#span { id=myspan }
	].
	
event(moused_over) ->
	wf:update(myspan, "The mouse is over the button!");
	
event(moused_out) ->
	wf:update(myspan, "");
	
event(clicked) ->
	wf:update(myspan, "You clicked the button!").