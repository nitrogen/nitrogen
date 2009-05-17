% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_rounded_panel).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, rounded_panel).

render(ControlID, Record) -> 
	Color = wf:to_list(Record#rounded_panel.color),
	TL = wf:f("<img src='/nitrogen/~s_tl.png' style='vertical-align: top;'>", [Color]),
	TR = wf:f("<img src='/nitrogen/~s_tr.png' style='vertical-align: top;'>", [Color]),
	BL = wf:f("<img src='/nitrogen/~s_bl.png' style='vertical-align: bottom;'>", [Color]),
	BR = wf:f("<img src='/nitrogen/~s_br.png' style='vertical-align: bottom;'>", [Color]),
	
	Terms = #table {
		class="rounded_panel " ++ Color ++ Record#rounded_panel.class,
		style=Record#rounded_panel.style,
		rows=[
			#tablerow { class="chrome", cells=[
				#tablecell { align=left, valign=top, text=TL, html_encode=false }, 
				#tablecell { text="" },
				#tablecell { align=right, valign=top, text=TR, html_encode=false }
			]},
			#tablerow { cells=#tablecell { colspan=3, class=content, body=Record#rounded_panel.body } },
			#tablerow { class="chrome", cells=[
				#tablecell { align=left, valign=bottom, text=BL, html_encode=false }, 
				#tablecell { text="" },
				#tablecell { align=right, valign=bottom, text=BR, html_encode=false }
			]}
		]
	},
	element_table:render(ControlID, Terms).