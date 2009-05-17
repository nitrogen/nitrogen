% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_lightbox).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, lightbox).

render(ControlID, Record) -> 
	Terms = #panel {
		class=lightbox,
		style="position: fixed; top: 0px; left: 0px; bottom: 0px; right: 0px;",
		body=[
			#panel { 			
				class=lightbox_background, 
				style="position: fixed; top: 0px; left: 0px; bottom: 0px; right: 0px; z-index: 98; background-color: #000000;"
			},
			#table { 
				style="position: fixed; top: 0px; left: 0px; width: 100%; height: 100%; z-index: 99; overflow:auto;", 
				rows=#tablerow {
					cells=#tablecell { align=center, valign=middle, style="vertical-align: middle;", body=Record#lightbox.body } 
				}
			}
		]
	},
	element_panel:render(ControlID, Terms).
