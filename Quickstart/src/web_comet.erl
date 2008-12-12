-module (web_comet).
-include ("wf.inc").
-export ([main/0, event/1]).

main() ->
	Title = "Comet Example",
	Body = #template { file=onecolumn, title=Title, headline=Title,
	section1=[
		#panel { id=myPanel },
		#button { text="Button", postback=button }
	]},
	
	wf:comet(fun() -> background_update(1) end),
	
	wf:render(Body).
	
event(_Event) -> 
	wf:flash("Pushed a button.");
	
event(_) -> ok.

background_update(Count) ->
	timer:sleep(1000),
	wf:update(myPanel, wf:to_list(Count)),
	wf:comet(fun() -> background_update(Count + 1) end).
	
