-module (web_samples_radio).
-include ("wf.inc").
-include ("google_chart.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Radio Button Example".
headline() -> "Radio Button Example".
right() -> linecount:render().

body() -> [

	#p{},
	#label { text="Radio Buttons" },
	#radiogroup { id=myRadio, body=[
		#radio { id=myRadio1, text="Option 1", value="1", postback={checked, 1}, checked=true }, #p{},
		#radio { id=myRadio2, text="Option 2", value="2", postback={checked, 2} }, #p{},
		#radio { id=myRadio3, text="Option 3", value="3", postback={checked, 3} }, #p{},
		#radio { id=myRadio4, text="Option 4", value="4", postback={checked, 4} }
	]},
	#p{},
	#button { text="Postback", postback=clicked_button }
].

event({checked, Number}) ->
	wf:flash(wf:f("You selected radio button ~p.", [Number])),
	ok;
	
event(clicked_button) ->
	
	wf:flash("Radio button " ++ wf:q(myRadio) ++ " is selected."),
	wf:flash("Is radio button 1 selected: " ++ wf:q(myRadio1)),
	ok; 

event(_) -> ok.