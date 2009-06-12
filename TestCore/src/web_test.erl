-module (web_test).
-include ("wf.inc").
-compile(export_all).

main(Context) ->
	% Switch to comet mode.
	Page = Context#context.page_context,
	Context1 = Context#context { 
		%page_context=Page#page_context { async_mode={poll, 1000} }
		page_context=Page#page_context { async_mode=comet }
	},

	Elements = [
		#textbox { id=textbox },
		#button { id=button, text="Click Me", postback=click },
		#panel { id=mypanel, body=[
			#panel { id=test1, body="Test1" },
			#span { id=test2, text="Test2" },
			#panel { id=test3, body=#panel { id=test4 } }
		]}
	],
	
	{ok, Context2} = wff:wire(#async { pool=count, scope=global, function=fun(Cx) -> background(1, Cx) end }, Context1),
	{ok, Elements, Context2}.
	
event(_Tag, Context) ->
	action_async:send_global(count, increment, Context).
	
background(Count, Context) ->
	receive 
		increment -> 
			{ok, Context1} = wff:update(mypanel, wff:to_list(Count), Context),
			{ok, Context2} = action_async:flush(Context1),
			background(Count + 1, Context2);
		_Other -> 
			background(Count, Context)
	end.
	