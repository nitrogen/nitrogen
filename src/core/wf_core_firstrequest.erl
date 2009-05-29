-module (wf_core_firstrequest).
-include ("wf.inc").
-export ([run/1]).

run(Context) ->
	% Run the request. This returns a new context.
	PageModule = Context#context.page_module,
	{ok, Data, NewContext} = PageModule:main(Context),
	{ok, NewContext#context { data=Data}}.
