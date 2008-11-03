-module (element_value).
-include ("wf.inc").
-compile(export_all).

render(ControlID, Record) -> 
	wf:f("<span id='~s' class='value ~s' style='~s'>~s</span>", [
		ControlID,
		Record#value.class,
		Record#value.style,
		wf:html_encode(Record#value.text, Record#value.html_encode)
	]).