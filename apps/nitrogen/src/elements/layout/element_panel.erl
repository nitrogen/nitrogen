% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_panel).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, panel).

render_element(Record) -> 
    wf_tags:emit_tag('div', Record#panel.body, [
        {class, ["panel", Record#panel.class]},
        {style, Record#panel.style}
    ]).
