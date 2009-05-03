-module (web_nbe_1_5).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	[
		#button { text="Button", actions=[
			#event { type=mouseover, postback=moused_over },
			#event { type=click, postback=clicked },
			#event { type=mouseout, postback=moused_out }
		]},
		#span { id=myspan }
	].
	
event(moused_over) ->
	wf:update(myspan, "The mouse is over the button!");
	
event(moused_out) ->
	wf:update(myspan, "");
	
event(clicked) ->
	wf:update(myspan, "You clicked the button!").