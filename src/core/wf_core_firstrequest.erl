-module (wf_core_firstrequest).
-include ("wf.inc").
-export ([run/1]).

run(Context) ->
	% Run the request. This returns a new context.
	PageModule = Context#context.page_module,
	?PRINT(PageModule),
	PageModule:main(Context).
