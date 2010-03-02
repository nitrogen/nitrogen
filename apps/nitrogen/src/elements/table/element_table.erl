% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_table).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, table).

render_element(Record) -> 
    Rows = Record#table.rows,

    wf_tags:emit_tag(table, Rows, [
        {border, 0},
        {cellpadding, 0},
        {cellspacing, 0},
        {class, [table, Record#table.class]},
        {style, Record#table.style}
    ]).
