% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_dropdown).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, dropdown).

render(ControlID, Record) -> 
	case Record#dropdown.postback of
		undefined -> ok;
		Postback -> wf:wire(ControlID, #event { type=change, postback=Postback })
	end,
	
	case Record#dropdown.value of 
		undefined -> ok;
		Value -> wf:set(ControlID, Value)
	end,
	
	Options=case Record#dropdown.options of
		undefined -> "";
		L -> [create_option(X, Record#dropdown.html_encode) || X <- L]
	end,

	wf_tags:emit_tag(select, Options, [
		{id, ControlID},
		{name, ControlID},
		{class, [dropdown, Record#dropdown.class]},
		{style, Record#dropdown.style}
	]).
		
create_option(X, HtmlEncode) ->
	SelectedOrNot = case X#option.selected of
		true -> selected;
		_ -> not_selected
	end,
	
	Content = wf:html_encode(X#option.text, HtmlEncode),
	Value = wf:html_encode(X#option.value, HtmlEncode),
	wf_tags:emit_tag(option, Content, [
		{value, Value},
		{SelectedOrNot, true}
	]).