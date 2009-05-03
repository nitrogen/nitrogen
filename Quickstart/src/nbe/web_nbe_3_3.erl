% ./src/nbe/web_nbe_3_3.erl
-module (web_nbe_3_3).
-include ("wf.inc").
-include ("is_a_z.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	wf:wire(mybutton, mytextbox, #validate { validators=[
		#is_a_z { text="Must start with 'A' and end with 'Z'." }
	]}),

	[
		#label { text="Enter some text that starts with 'A' and ends with 'Z':" },
		#textbox { id=mytextbox, next=mybutton },
		#p{},
		#button { id=mybutton, text="Button", postback=show_backwards_alert },
		#p{},
		#span { id=myspan }
	].
	
event(show_backwards_alert) ->
	[Text] = wf:q(mytextbox),
	wf:update(myspan, "You entered: " ++ Text).