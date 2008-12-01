-module (web_index).
-include ("wf.inc").
-export ([main/0, event/1]).

main() ->
	Body = #body { body=#panel { style="margin: 50px;", body=[
		#h1 { text="Nitrogen Example Pages" },
		#h3 { text="Templates" },
		#link { text="No Template", url="/web/template_none" }, #br{},
		#link { text="Simple Template", url="/web/template_simple" },
		#p{},
		
		#h3 { text="Using Controls" },
		#link { text="Some Simple Controls", url="/web/simplecontrols" }, #br{},
		#link { text="Postbacks", url="/web/postback" }, #br{},
		#link { text="AJAX Replacement", url="/web/ajax" }, #br{},
		#p{},
		
		#h3 { text="Data Binding" },
		#link { text="Simple (List-Based) Binding", url="/web/binding1" }, #br{},
		#link { text="Record-Based Binding", url="/web/binding2" }, #br{},
		#link { text="Key/Value Pair Binding", url="/web/binding3" },
		#p{}
	]}},
	wf:render(Body).
	
event(go) ->
	wf:flash("Hello there"),
	wf:update(test, "This is a test.");
	
event(_) -> ok.