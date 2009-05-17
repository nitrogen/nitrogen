% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_spinner).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, spinner).

render(_ControlID, Record) -> 
	wf:wire(spinner, #hide{}),
	Terms = #panel {
		id=spinner,
		class=wf:f("spinner ~s", [Record#spinner.class]),
		style=Record#spinner.style,
		body=#image { image=Record#spinner.image }
	},
	
	wf:render(Terms).
	
