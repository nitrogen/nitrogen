-module (web_nbe_2_5).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	wf:wire(mybutton, mytextbox, #validate { validators=[
		#is_required { text="Required" },
		#js_custom { text="Must start with 'A'", function="function(v) { return (v.toUpperCase()[0] == 'A'); }" },
		#custom { text="Must end with 'Z'", function=fun(_Tag, Value) -> string:to_upper(hd(lists:reverse(Value))) == $Z end }
	]}),

	[
		#label { text="Enter some text that starts with 'A' and ends with 'Z':" },
		#textbox { id=mytextbox, next=mybutton },
		#p{},
		#button { id=mybutton, postback=go },
		#p{},
		#span { id=myspan }
	].
	
event(go) -> 
	[Text] = wf:q(mytextbox),
	wf:update(myspan, "You entered: " ++ Text).