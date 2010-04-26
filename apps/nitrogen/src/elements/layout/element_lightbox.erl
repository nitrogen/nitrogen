% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_lightbox).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, lightbox).

render_element(Record) -> 
    ?PRINT(Record#lightbox.id),
    Panel = #panel {
        id=Record#lightbox.id,
        anchor=Record#lightbox.anchor,
        class=[lightbox, Record#lightbox.class],
        style="position: fixed; top: 0px; left: 0px; bottom: 0px; right: 0px; " ++ wf:to_list(Record#lightbox.style),
        body=[
            #panel { 			
                class=lightbox_background, 
                style="position: fixed; top: 0px; left: 0px; bottom: 0px; right: 0px; z-index: 98; background-color: #000000;"
            },
            #table { 
                style="position: fixed; top: 0px; left: 0px; width: 100%; height: 100%; z-index: 99; overflow:auto;", 
                rows=#tablerow {
                    cells=#tablecell { align=center, valign=middle, style="vertical-align: middle;", body=[
                        "<center><table><tr><td>",
                        Record#lightbox.body,
                        "</td></tr></table></center>"
                    ]} 
                }
            }
        ]
    },
    element_panel:render_element(Panel).
