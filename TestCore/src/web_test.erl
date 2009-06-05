-module (web_test).
-include ("wf.inc").
-compile(export_all).

main(Context) ->
	Elements = [
		#button { text="Click Me", postback=click },
		#panel { id=mypanel, body=[
			#panel { id=test1, body="Test1" },
			#span { id=test2, text="Test2" },
			#panel { id=test3, body=#panel { id=test4 } }
		]}
	],
	
	F = fun() -> ?PRINT(sleeping), timer:sleep(2000), ?PRINT(done_sleeping) end,
	{ok, Context1} = wff:wire(#continue { tag=mytag, function=F }, Context),
	{ok, Elements, Context1}.
	
event(_Tag, Context) ->
	{ok, _Context1} = wff:update(mypanel, "Clicked", Context).
	
continue(_Tag, _Result, Context) ->
	{ok, _Context1} = wff:update(mypanel, "Continued", Context).