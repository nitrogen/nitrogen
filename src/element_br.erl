-module (element_br).
-compile(export_all).
-include ("wf.inc").

render(ControlID, Record) -> 
	wf:f("<br id='~s' class='p ~s' style='~s'>", [
		ControlID, 
		Record#br.class,
		Record#br.style
	]).