-module (element_textarea).
-include ("wf.inc").
-compile(export_all).

render(ControlID, Record) -> 
	wf:f("<textarea id='~s' class='textarea ~s' style='~s' name='~s'>~s</textarea>", [
		ControlID, 
		Record#textarea.class,
		Record#textarea.style,
		ControlID, 
		wf:html_encode(Record#textarea.text, Record#textarea.html_encode)
	]).
	
