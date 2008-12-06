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
		replacements(wf:to_list(B)),
		"</pre>"
	].
	
replacements([]) -> [];
replacements([$<|T]) -> [$&,$l,$t,$;|replacements(T)];
replacements([$\t|T]) -> [$\s, $\s|replacements(T)];
replacements([H|T]) -> [H|replacements(T)].
	
event(_) -> ok.