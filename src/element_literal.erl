-module (element_literal).
-compile(export_all).
-include ("wf.inc").

render(_ControlID, Record) -> 
	wf:html_encode(Record#literal.text, Record#literal.html_encode).