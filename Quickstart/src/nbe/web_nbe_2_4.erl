-module (web_nbe_2_4).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	wf:wire(mybutton, mytextbox, #validate { validators=[
		#is_required { text="Required" },
		#max_length { text="Too many characters", length=10 }
	]}),

	[
		#label { text="Enter some text" },
		#textbox { id=mytextbox },
		#p{},
		#button { id=mybutton, postback=go },
		#p{},
		#span { id=myspan }
	].
	
event(go) -> 
	[Text] = wf:q(mytextbox),
	wf:update(myspan, "You entered: " ++ Text).