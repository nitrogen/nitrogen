-module (web_samples_windex).
-include ("wf.inc").
-compile(export_all).

main() -> #windex {}.

body() -> [	
	#textbox { id=myTextbox, next=myButton, text="" },
	#button { id=myButton, text="Press Me", postback=button_pressed },
	#p{},
	#panel { id=myPanel }
].	

event(_) ->
	[Text] = wf:q(myTextbox),
	?LOG("The user entered text: ~s", [Text]),
	{Year, Month, Day} = date(),
	{Hour, Minute, Second} = time(),
	wf:update(myPanel, [
		wf:f("You have entered: ~s<br>", [Text]),
		wf:f("Node: ~s.~n<br>", [node()]),
		wf:f("Date: ~p-~p-~p<br>", [Year, Month, Day]),
		wf:f("Time: ~p:~p:~p<br>", [Hour, Minute, Second])
	]),
	wf:wire(myPanel, #effect { effect=highlight}),
	ok.