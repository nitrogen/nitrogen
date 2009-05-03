-module (web_nbe_2_2).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> [
	#button { text="Button", postback=pulsate },
	#span { id=myspan, text="Example text." }
].

event(pulsate) ->
	wf:wire(myspan, #effect { target=myspan, effect=pulsate }).