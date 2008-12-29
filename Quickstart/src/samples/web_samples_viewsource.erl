-module (web_samples_viewsource).
-include ("wf.inc").
-compile(export_all).

main() ->
	[Module] = wf:q(module),
	
	{Module1, _} = case Module of
		"web_samples" ++ _ -> wf_utils:path_to_module(Module);
		_ -> {undefined, undefined}
	end,
	
	get_source(Module1).
	
get_source(undefined) -> "";
get_source(Module) ->		
	CompileInfo = Module:module_info(compile),
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