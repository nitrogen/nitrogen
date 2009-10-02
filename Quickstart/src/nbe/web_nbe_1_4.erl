-module (web_nbe_1_4).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	[
		#button { text="Button 1", postback=button1 },
		#span { id=myspan1 },
		#p{},
		#button { text="Button 2", actions=#event { type=click, postback=button2 } },
		#span { id=myspan2 }
	].
	
event(button1) -> 
	wf:update(myspan1, "You clicked Button 1!");

event(button2) -> 
	wf:update(myspan2, "You clicked Button 2!").
