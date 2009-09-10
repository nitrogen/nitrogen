% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_panel).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, panel).

render_element(HtmlID, Record) -> 
	wf_tags:emit_tag('div', Record#panel.body, [
	    {id, HtmlID},
	    {class, ["panel", Record#panel.class]},
	    {style, Record#panel.style}
	]).