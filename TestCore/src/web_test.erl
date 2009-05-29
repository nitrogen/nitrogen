-module (web_test).
-include ("wf.inc").
-compile(export_all).

main(Context) ->
	Elements = [
		#panel { id=panel1, body=[
			"<i>Panel1</i>",
			#h1 { id=header1, text="This is a test11."},
			#h2 { id=header2, text="This is a test12." }
		]},
		#panel { id=panel2, body=[
			"<i>Panel2</i>",
			#h1 { id=header1, text="This is a test21."},
			#h2 { id=header2, text="This is a test22." }
		]}
	],
	{ok, Context1} = wff:wire(header1, [
		#effect { effect=appear },
		#effect { effect=hello }
	], Context),
	{ok, Context2} = wff:wire(header2, #effect { effect=highlight }, Context1),
	{ok, Context3} = wff:update(panel1, #span { text="Stuff" }, Context2),
	{ok, Elements, Context3}.