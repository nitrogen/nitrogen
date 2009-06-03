-module (web_test).
-include ("wf.inc").
-compile(export_all).

main(Context) ->
	{ok, Value, Context1} = wff:q(test, Context),
	?PRINT(Value),
	Elements = [
		#button { text="Click Me", postback=click },
		#panel { id=mypanel, body="My Body" }
	],
	{ok, Elements, Context1}.
	
event(_Tag, Context) ->
	{ok, Context1} = wff:update(mypanel, "Clicked", Context),
	{ok, Context1}.