% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_singlerow).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, singlerow).

render(ControlID, Record) -> 
	Terms = #table {
		class = "singlerow " ++ wf:to_list(Record#singlerow.class),
		style = Record#singlerow.style,
		rows = #tablerow {
			cells = Record#singlerow.cells
		}
	},
	element_table:render(ControlID, Terms).
