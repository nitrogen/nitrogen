% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_p).
-compile(export_all).
-include_lib ("wf.hrl").

reflect() -> record_info(fields, p).

render_element(Record) -> 
    wf_tags:emit_tag(p, Record#p.body, [
        {class, [p, Record#p.class]},
        {style, Record#p.style}
    ]).

