-module (web_test).
-include ("wf.inc").
-compile(export_all).

main() ->
	wf:switch_to_comet(),
	
	Elements = [
		#textbox { id=textbox },
		#button { id=button, text="Click Me", postback=click },
		#panel { id=mypanel, body=[
			#panel { id=test1, body="Test1" },
			#span { id=test2, text="Test2" },
			#panel { id=test3, body=#panel { id=test4 } }
		]}
	],
	
	wf:wire(#async { pool=count, scope=global, function=fun() -> background(1) end }),
	Elements.
	
event(_Tag) ->
	wf:send_global(count, increment).
	
background(Count) ->
	receive 
		increment -> 
			wf:update(mypanel, wf:to_list(Count)),
			wf:flush(),
			background(Count + 1);
		_Other -> 
			background(Count)
	end.
	