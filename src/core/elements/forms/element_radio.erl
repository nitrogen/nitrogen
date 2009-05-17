% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_radio).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, radio).

render(ControlID, Record) -> 
	CheckedOrNot = case Record#radio.checked of
		true -> checked;
		_ -> not_checked
	end,

	case Record#radio.postback of
		undefined -> ok;
		Postback -> wf:wire(ControlID, #event { type=change, postback=Postback })
	end,

	Content = wf:html_encode(Record#radio.text, Record#radio.html_encode),

	[
		%% Checkbox...
		wf_tags:emit_tag(input, [
			{id, ControlID}, 
			{name, Record#radio.name},
			{value, Record#radio.value},
			{type, radio},
			{class, [radio, Record#radio.class]},
			{style, Record#radio.style},
			{CheckedOrNot, true}
		]),

		%% Label for Radio...
		wf_tags:emit_tag(label, Content, [
			{for, ControlID}
		])
	].
