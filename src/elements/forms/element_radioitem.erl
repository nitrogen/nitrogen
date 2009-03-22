% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_radioitem).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, radioitem).

render(ControlID, Record) -> 
	CheckedOrNot = case Record#radioitem.checked of
		true -> checked;
		_ -> not_checked
	end,

	case Record#radioitem.postback of
		undefined -> ok;
		Postback -> wf:wire(ControlID, #event { type=change, postback=Postback })
	end,

	Content = wf:html_encode(Record#radioitem.text, Record#radioitem.html_encode),

	[
		%% Checkbox...
		wf_tags:emit_tag(input, [
			{id, ControlID}, 
			{name, Record#radioitem.name},
			{value, Record#radioitem.value},
			{type, radio},
			{class, [radio, Record#radioitem.class]},
			{style, Record#radioitem.style},
			{CheckedOrNot, true}
		]),

		%% Label for Radio...
		wf_tags:emit_tag(label, Content, [
			{for, ControlID}
		])
	].
