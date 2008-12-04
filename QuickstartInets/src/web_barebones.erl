-module (web_barebones).
-include ("wf.inc").
-export ([main/0, event/1]).

main() ->
	Title = "Bare Bones Page",
	Body = #template { file=onecolumn, title=Title, headline=Title, section1=[
		"Nothing to see here."
	]},
	wf:render(Body).
	
event(_) -> ok.