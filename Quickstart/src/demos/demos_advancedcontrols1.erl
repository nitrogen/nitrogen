-module (demos_advancedcontrols1).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "In-Place Textbox".

headline() -> "In-Place Textbox".


left() -> 
    [
        "
        The In-Place textbox allows a user to edit a field or value.
        ",
        linecount:render()
    ].

right() -> [
	#inplace_textbox { text="Sample Text 1." }, 
	
	#p{},			
	#inplace_textbox { text="Sample Text 2." }, 
	
	#p{},			
	#inplace_textbox { text="Sample Text 3." }
].
	
inplace_textbox_event(_Tag, Value) ->
	wf:wire(#alert { text=wf:f("You entered: ~s", [Value]) }),
	Value.
