-module (web_test).
-include ("wf.inc").
-compile(export_all).

main(Context) ->
	Elements = [
		"Hello there",
		#button { text="Click Me", postback=click },
		#panel { id=mypanel, body=[
			#panel { id=test1, body="Test1" },
			#span { id=test2, text="Test2" },
			#panel { id=test3, body=#panel { id=test4 } }
		]}
	],
	{ok, Elements, Context}.
	
event(_Tag, Context) ->
	{ok, _Context1} = wff:update(mypanel, "Clicked", Context).