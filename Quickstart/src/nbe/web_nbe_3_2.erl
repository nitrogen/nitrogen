% ./src/nbe/web_nbe_3_2.erl
-module (web_nbe_3_2).
-include ("wf.inc").
-include ("backwards_alert.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	[
		#label { text="Enter some text to display backwards:" },
		#textbox { id=mytextbox, next=mybutton },
		#p{},
		#button { id=mybutton, text="Button", postback=show_backwards_alert }
	].
	
event(show_backwards_alert) ->
	[Text] = wf:q(mytextbox),
	wf:wire(#backwards_alert { text=Text }).