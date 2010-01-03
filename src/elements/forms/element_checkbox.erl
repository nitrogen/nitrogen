% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_checkbox).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, checkbox).

render_element(Record) -> 
	Anchor = Record#checkbox.anchor,
	CheckedOrNot = case Record#checkbox.checked of
		true -> checked;
		_ -> not_checked
	end,
	case Record#checkbox.postback of
		undefined -> ignore;
		Postback -> wf:wire(Record#checkbox.id, #event { type=change, postback=Postback, delegate=Record#checkbox.delegate })
	end,
	
	Text = wf:html_encode(Record#checkbox.text, Record#checkbox.html_encode),
	[
		% Checkbox...
		wf_tags:emit_tag(input, [
			{name, Anchor},
			{type, checkbox},
			{class, [checkbox, Record#checkbox.class]},
			{style, Record#checkbox.style},
			{CheckedOrNot, true}
		]),

		% Label for Checkbox...
		wf_tags:emit_tag(label, Text, [
			{for, Anchor}
		])
	].