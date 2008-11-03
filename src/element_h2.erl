-module (element_h2).
-include ("wf.inc").
-compile(export_all).

render(ControlID, Record) -> 
	wf:f("<h2 id='~s' class='h2 ~s' style='~s'>~s</h2>", [
		ControlID,
		Record#h2.class,
		Record#h2.style,
		wf:html_encode(Record#h2.text, Record#h2.html_encode)
	]).