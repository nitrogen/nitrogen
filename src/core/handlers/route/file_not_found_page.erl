-module (file_not_found_page).
-include ("wf.inc").
-export ([main/1]).

main(Context) ->
	Page = Context#context.page_context,
	PathInfo = Page#page_context.path_info,
	{ok, Context1} = wff:info("File not found: ~p", [PathInfo], Context),
	
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