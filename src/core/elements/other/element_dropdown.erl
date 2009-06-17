% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_dropdown).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, dropdown).

render_element(HtmlID, Record, Context) -> 
	{ok, Context1} = case Record#dropdown.postback of
		undefined -> {ok, Context};
		Postback -> wff:wire(Record#dropdown.id, #event { type=change, postback=Postback })
	end,

	% TODO -
	% case Record#dropdown.value of 
	% 	undefined -> ok;
	% 	Value -> wff:set(HtmlID, Value)
	% end,
	
	Options=case Record#dropdown.options of
		undefined -> "";
		L -> [create_option(X, Record#dropdown.html_encode) || X <- L]
	end,

	Elements = wf_tags:emit_tag(select, Options, [
		{id, HtmlID},
		{id, HtmlID},
		{class, [dropdown, Record#dropdown.class]},
		{style, Record#dropdown.style}
	]),
	{ok, Elements, Context1}.
		
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