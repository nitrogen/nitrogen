-module (PAGE).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"PAGE".

body() ->
	#label{text="PAGE body."}.
	
event(_) -> ok.