%%% Datepicker Control Element.
%%% Copyright (c) 2009 Torbjorn Tornkvist
%%% See MIT-LICENSE for the Nitrogen Web Framework for Erlang

-module (element_datepicker_textbox).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, datepicker_textbox).

render_element(HtmlID, Record) -> 
	Class   = Record#datepicker_textbox.class,
	Options = action_jquery_effect:options_to_js(Record#datepicker_textbox.options),

	Textbox = #textbox {
	 id          = Record#datepicker_textbox.id,
	 class       = "datepicker_textbox "  ++ wf:to_list(Class),
	 style       = Record#datepicker_textbox.style,
	 text        = Record#datepicker_textbox.text,
	 html_encode = Record#datepicker_textbox.html_encode
	},

	Script = wf:f("Nitrogen.$datepicker(obj('me'), ~s);", [Options]),
	wf:wire(Record#datepicker_textbox.id, #script { script=Script }),

	element_textbox:render_element(HtmlID, Textbox).