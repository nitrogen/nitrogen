-module (web_nbe_1_3).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> [
	#button { text="Click Me!", postback=click_me },
	#span { id=myspan }
].

event(_) -> 
	wf:update(myspan, "You clicked the button!").
			