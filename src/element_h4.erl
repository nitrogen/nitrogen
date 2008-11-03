-module (element_h4).
-include ("wf.inc").
-compile(export_all).

render(ControlID, Record) -> 
	wf:f("<h4 id='~s' class='h4 ~s' style='~s'>~s</h4>", [
		ControlID,
		Record#h4.class,
		Record#h4.style,
		Record#h4.text
	]).