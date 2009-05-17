-module (wf_context).
-export ([
	make_context/2,
	to_html/1,
	to_javascript/1
]).

make_context(Module, RequestBridge) ->
	#context {
		page_module = Module,
		path_info = PathInfo,
		request = RequestBridge,
		response = 
		page_area = page_state_area:new([])
	}.
	
to_html(Context) ->
	
to_javascript(Context) ->
	