-module (element_panel).
-include ("wf.inc").
-compile(export_all).

render(ControlID, Record) -> 
	[
		wf:f("<div id='~s' class='panel ~s' style='~s'>", [
			ControlID,
			Record#panel.class,
			Record#panel.style
		]),
		wf:render(Record#panel.body),
		"</div>"
	].