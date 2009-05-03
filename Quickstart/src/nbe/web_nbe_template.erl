-module (web_nbe_template).
-include ("wf.inc").
-compile(export_all).

main() ->
	wf:set_content_type("text/plain"),
	{ok, B} = file:read_file("./wwwroot/nbe.html"),
	wf:to_list(B).