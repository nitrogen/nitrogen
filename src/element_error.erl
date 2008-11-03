-module (element_error).
-include ("wf.inc").
-compile(export_all).

render(ControlID, Record) -> 
	Terms = #span {
		class="error " ++ wf:to_list(Record#error.class),
		style=Record#error.style,
		text=Record#error.text,
		html_encode=Record#error.html_encode
	},
	element_span:render(ControlID, Terms).