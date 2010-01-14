-module (web_samples_remove).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "AJAX Remove Example".
headline() -> "AJAX Remove Example".
right() -> linecount:render().

body() -> [
	#panel { id=mainPanel, body=[
		"
			When you click on the button below, the page will use wf:remove(mainPanel) to 
			remove this #panel.
		",
		#p{},
		#button { postback=remove }
	]},
	#p{},
	"Reload to reset the page."
].
	
event(remove) ->
	wf:remove(mainPanel);
	
event(_) -> ok.