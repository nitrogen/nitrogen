-module (element_hidden).
-include ("wf.inc").
-compile(export_all).

render(ControlID, Record) -> 
	wf:f("<input id='~s' type='hidden' name='~s' value=\"~s\" />", [
		ControlID, 
		ControlID, 
		wf:html_encode(Record#hidden.text, Record#hidden.html_encode)
	]).
	
