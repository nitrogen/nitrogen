-module (web_template_simple).
-include ("wf.inc").
-export ([main/0, event/1]).

main() ->
	Body = #template { title="Simple Template", file='_template', section1=[
		#h1 { text="Simple Template Example" },
		#span { text="
			This page has a simple template, found in <b>/content/web_content/_template.html</b>.
		", html_encode=false}
	]},
	wf:render(Body).
	
event(go) ->
	wf:flash("Hello there"),
	wf:update(test, "This is a test.");
	
event(_) -> ok.