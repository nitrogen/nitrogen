-module (web_test1).
-include ("wf.inc").
-export ([main/0, event/1]).

main() ->
	Body = #template { title="Test 1", file='_template', section1=[
		#panel { style="margin: 50px;", body=[
			#textbox { id=myTextbox },
			#button { text="Go", postback=go },
			#br{},
			#flash { id=flash },
			#panel { id=test },
			"Hello there"
		]}
	]},
	wf:render(Body).
	
event(go) ->
	wf:flash("Hello there"),
	wf:update(test, "This is a test.");
	
event(_) -> ok.