% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_windex).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, windex).

render(_ControlID, _Record) ->
	% Prevent loops.
	case wf:state(template_was_called) of
		true -> throw("Calling a template from a template.");
		_ -> ignore
	end,
	wf:state(template_was_called, true),
	
	% Evaluate the template.
 	PageModule = wf_platform:get_page_module(),
	Body = wf:render(PageModule:body()),
	
	% Return the response.
	[
		wf:f("Nitrogen.$lookup('~s').$update(\"~s\");", [get(current_id), wf_utils:js_escape(Body)]),
		wf_script:get_script()
	].