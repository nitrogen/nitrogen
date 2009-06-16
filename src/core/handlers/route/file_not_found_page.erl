-module (file_not_found_page).
-include ("wf.inc").
-export ([main/1]).

main(Context) ->
	Page = Context#context.page_context,
	PathInfo = Page#page_context.path_info,
	{ok, Context1} = wff:info("File not found: ~p", [PathInfo], Context),
	Elements = [
		"404 - Not found."
	],
	{ok, Elements, Context1}.