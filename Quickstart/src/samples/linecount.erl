-module (linecount).
-include ("wf.inc").
-export ([render/0]).

render() ->
	CurrentModule = wf:get_page_module(),
	{_CurrentTotal, CurrentActive} = line_count(CurrentModule),
	{_BareTotal, BareActive} = line_count(web_samples_barebones),
  ActiveLines = CurrentActive - BareActive,
	
	Terms = [
		#p{},
		#span { class=stats, text=wf:f("This page clocks in at <span class=count>~p</span> lines of Nitrogen code.", [ActiveLines]), html_encode=false },
		#p{},
		#link { text="View source.", url=wf:f("/web/samples/viewsource?module=~s", [CurrentModule]) }
	],
	wf:render(Terms).
	
line_count(Module) ->
	CompileInfo = Module:module_info(compile),
	Source = proplists:get_value(source, CompileInfo),
	{ok, B} = file:read_file(Source),
	L1 = binary_to_list(B),
	L2 = tabs_to_spaces(L1),
	Lines1 = string:tokens(L2, "\n"),
	Lines2 = [string:strip(X) || X <- Lines1],
	Lines3 = [X || X <- Lines2, X /= ""],
	{length(Lines1), length(Lines3)}.

tabs_to_spaces([]) -> [];
tabs_to_spaces([$\t|T]) -> [$\s|tabs_to_spaces(T)];
tabs_to_spaces([H|T]) -> [H|tabs_to_spaces(T)].