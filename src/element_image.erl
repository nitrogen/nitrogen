-module (element_image).
-include ("wf.inc").
-compile(export_all).

render(ControlID, Record) -> 
	wf:f("<img id='~s' class='span ~s' style='~s' src='~s'>", [
		ControlID,
		Record#image.class,
		Record#image.style,
		Record#image.image
	]).