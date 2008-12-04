-module (web_advancedcontrols1).
-include ("wf.inc").
-include ("google_chart.inc").
-export ([main/0, event/1, inplace_textbox_event/2]).

main() ->
	Title = "Edit-In-Place Textbox Example",
	Body = #template { file=onecolumn, title=Title, headline=Title,
	section1=[
	
			#inplace_textbox { text="Sample Text 1." }, 
			#p{},			
			#inplace_textbox { text="Sample Text 2." }, 
			#p{},			
			#inplace_textbox { text="Sample Text 3." }, 
			#p{}

	]},
	wf:render(Body).
	
event(_) -> ok.

inplace_textbox_event(_Tag, Value) ->
	wf:wire(#alert { text=wf:f("You entered: ~s", [Value]) }),
	Value.