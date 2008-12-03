-module (web_viewsource).
-include ("wf.inc").
-export ([main/0, event/1]).

main() ->
	[Module] = wf:q(module),
	Module1 = wf:to_atom(Module),
	CompileInfo = Module1:module_info(compile),
	Source = proplists:get_value(source, CompileInfo),
	{ok, B} = file:read_file(Source),
	[
		"<pre>",
		replace_tabs(wf:to_list(B)),
		"</pre>"
	].
	
replace_tabs([]) -> [];
replace_tabs([$\t|T]) -> [$\s, $\s|replace_tabs(T)];
replace_tabs([H|T]) -> [H|replace_tabs(T)].
	

event(_) -> ok.