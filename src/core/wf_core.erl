-module (wf_core).
-include ("wf.inc").
-include ("simplebridge.hrl").
-export ([
	run/1,
	default_includes/0
]).

% nitrogen_core - 
% --
% Render a single Nitrogen page or inline application. This can be called
% from other Erlang web frameworks or resource servers, such as WebMachine, 
% Erlang Web, ErlyWeb, etc.

run(Context) ->
	PageModule = Context#context.page_module, 
	
	case is_first_request(Context#context.request) of
		true -> nitrogen_core_firstrequest:run(PageModule, Context);			
		_ -> nitrogen_core_postback:run(PageModule, Context)
	end.

is_first_request(_Context) -> true.
	
default_includes() ->
	[
		"<script src='/nitrogen/jquery.js' type='text/javascript' charset='utf-8'></script>",
		"<script src='/nitrogen/jquery-ui.js' type='text/javascript' charset='utf-8'></script>",
		"<script src='/nitrogen/livevalidation.js' type='text/javascript' charset='utf-8'></script>",
		"<script src='/nitrogen/nitrogen.js' type='text/javascript' charset='utf-8'></script>",
		"<link rel='stylesheet' href='/nitrogen/jquery-ui/jquery-ui.css' type='text/css' media='screen' charset='utf-8'>",
		"<link rel='stylesheet' href='/nitrogen/elements.css' type='text/css' media='screen' charset='utf-8'>"
	].