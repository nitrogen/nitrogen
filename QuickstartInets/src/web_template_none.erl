-module (web_template_none).
-include ("wf.inc").
-export ([main/0, event/1]).

main() ->
	Body = #body { title="No Template", body=[
	
		#h1 { text="No Template Example" },
		#span { text="
			This page does not have a surrounding page template.
		"}
		
	]},
	wf:render(Body).
	
event(_) -> ok.