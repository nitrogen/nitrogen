-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"Welcome to SKEL".
	
body() ->
	#label{text="Example Body."}.
	
event(_) -> ok.