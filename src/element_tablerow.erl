-module (element_tablerow).
-include ("wf.inc").
-compile(export_all).

render(ControlID, Record) -> 
	[
		wf:f("<tr id='~s' class='tablerow ~s' style='~s'>", [
			ControlID,
			Record#tablerow.class,
			Record#tablerow.style
		]),
		wf:render(Record#tablerow.cells),
		"</tr>"
	].