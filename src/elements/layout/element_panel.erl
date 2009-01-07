% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_panel).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, panel).

render(ControlID, Record) -> 
	Content = wf:render(Record#panel.body),
	wf_tags:emit_tag('div', Content, [
	    {id, ControlID},
	    {class, ["panel", Record#panel.class]},
	    {style, Record#panel.style}
	]).