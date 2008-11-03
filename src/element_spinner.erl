-module (element_spinner).
-include ("wf.inc").
-compile(export_all).

render(_ControlID, Record) -> 
	wf:wire(spinner, "obj(me).hide();"),
	Terms = #panel {
		id=spinner,
		class=wf:f("spinner ~s", [Record#spinner.class]),
		style=Record#spinner.style,
		body=#image { image=Record#spinner.image }
	},
	
	wf:render(Terms).
	
