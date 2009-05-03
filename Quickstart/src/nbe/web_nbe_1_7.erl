-module (web_nbe_1_7).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	[
		#button { text="One", postback={button, myspan1, "You clicked One!"}},
		#span { id=myspan1 },
		#p{},

		#button { text="Two", postback={button, myspan2, "You clicked Two!"}},
		#span { id=myspan2 },
		#p{},

		#button { text="Three", postback={button, myspan3, "You clicked Three!"}},
		#span { id=myspan3 }
	].
	
event({button, SpanID, Text}) ->
	wf:update(SpanID, Text).
