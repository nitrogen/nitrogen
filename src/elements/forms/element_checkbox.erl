% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_checkbox).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, checkbox).

render(ControlID, Record) -> 
	CheckedOrNot = case Record#checkbox.checked of
		true -> checked;
		_ -> not_checked
	end,
	case Record#checkbox.postback of
		undefined -> ok;
		Postback -> wf:wire(ControlID, #event { type=change, postback=Postback })
	end,
	
	Content = wf:html_encode(Record#checkbox.text, Record#checkbox.html_encode),
	[
		% Checkbox...
		wf_tags:emit_tag(input, [
			{id, ControlID}, 
			{name, ControlID},
			{type, checkbox},
			{class, [checkbox, Record#checkbox.class]},
			{style, Record#checkbox.style},
			{CheckedOrNot, true}
		]),

		% Label for Checkbox...
		wf_tags:emit_tag(label, Content, [
			{for, ControlID}
		])
	].