-module (web_samples_replace).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "AJAX Replace Example".
headline() -> "AJAX Replace Example".
right() -> linecount:render().

body() -> [
	#panel { id=mainPanel, body=[
		"
			When you click on the button below, the page will use wf:replace(mainPanel, Message) to 
			replace this #panel with some other html. Unlike wf:update/2, wf:replace/2 removes
			the original element.
		",
		#p{},
		#button { postback=replace }
	]}
].
	
event(replace) ->
	Elements = #panel { id=replacementPanel, body=[
		"See!",
		#p{},
		#button { text="Reset", postback=reset }
	]},
	wf:replace(mainPanel, Elements);
	
event(reset) ->
	wf:replace(replacementPanel, body());

event(_) -> ok.
