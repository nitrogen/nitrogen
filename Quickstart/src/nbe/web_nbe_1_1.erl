-module (web_nbe_1_1).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> [
	#span { text="Hello, World!" }
].