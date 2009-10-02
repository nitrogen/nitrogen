-module (web_nbe_2_1).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	#panel { style="text-align: center;", body="Pause and reflect." }.
