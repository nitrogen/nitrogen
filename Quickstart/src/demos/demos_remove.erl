-module (demos_remove).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html", bindings=[
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
