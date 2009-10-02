-module (web_nbe_2_3).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> [
	#button { text="Button", actions=[
		#event { type=click, actions=#effect { target=myspan, effect=pulsate }}
	]},
	#span { id=myspan, text="Example text." }
].