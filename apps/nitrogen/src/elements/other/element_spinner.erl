% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_spinner).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, spinner).

render_element(Record) -> 
    wf:wire(spinner, #hide{}),
    Terms = #panel {
        id=Record#spinner.id,
        anchor=Record#spinner.anchor,
        class=[spinner, Record#spinner.class],
        style=Record#spinner.style,
        body=#image { image=Record#spinner.image }
    },

    wf:render(Terms).

