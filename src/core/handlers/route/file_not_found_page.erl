-module (file_not_found_page).
-include ("wf.inc").
-export ([main/1]).

main(Context) ->
	{ok, Context1} = wff:info("File not found: ~p", [Context#context.path_info], Context),
	
	Elements = [
		#h1 { text="Page not found." },
		#panel { body=[
			#span { text="1"},
			#span { text="2"},
			#span { text="3"},
			#span { text="4"}
		]}
	],
	{ok, Elements, Context1}.