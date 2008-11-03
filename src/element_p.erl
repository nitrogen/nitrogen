-module (element_p).
-compile(export_all).
-include ("wf.inc").

render(ControlID, Record) -> 
	wf:f("<p id='~s' class='p ~s' style='~s' />", [
		ControlID, 
		Record#p.class,
		Record#p.style
	]).