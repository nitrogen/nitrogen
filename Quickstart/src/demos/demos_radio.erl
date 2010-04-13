-module (demos_radio).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Radio Button".

headline() -> "Radio Button".

left() -> 
    [
        "
        <p>
        The <code>#radiogroup{}</code> and <code>#radio{}</code>
        elements let you create a group of radio buttons and detect
        when a user has made a new selection.
        ",
        linecount:render()
    ].

right() -> 
    [
        #flash {},
        #p{},
	#radiogroup { id=myRadio, body=[
            #radio { id=myRadio1, text="Option 1", value="1", postback={checked, 1}, checked=true }, #br{},
            #radio { id=myRadio2, text="Option 2", value="2", postback={checked, 2} }, #br{},
            #radio { id=myRadio3, text="Option 3", value="3", postback={checked, 3} }, #br{},
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
	
    IsRadio1 = (wf:q(myRadio1) /= undefined),
    wf:flash("Is radio button 1 selected: " ++ atom_to_list(IsRadio1)),
    ok; 

event(_) -> ok.
