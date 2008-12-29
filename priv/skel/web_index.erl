-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"A New Webpage".
	
body() ->
	#label{text="This is my body."}.
	
event(_) -> ok.