-module (web_samples_advancedcontrols1).
-include ("wf.inc").
-include ("google_chart.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Edit-In-Place Textbox Example".
headline() -> "Edit-In-Place Textbox Example".
right() -> linecount:render().

body() -> [
	#p{},
	#inplace_textbox { text="Sample Text 1." }, 
	
	#p{},			
	#inplace_textbox { text="Sample Text 2." }, 
	
	#p{},			
	#inplace_textbox { text="Sample Text 3." }
].
	
event(_) -> ok.

inplace_textbox_event(_Tag, Value) ->
	wf:wire(#alert { text=wf:f("You entered: ~s", [Value]) }),
	Value.