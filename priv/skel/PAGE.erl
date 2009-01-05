-module (MODULE_NAME).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"MODULE_NAME".

body() ->
	#label{text="MODULE_NAME body."}.
	
event(_) -> ok.