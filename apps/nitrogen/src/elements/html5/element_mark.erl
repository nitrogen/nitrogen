% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_mark).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, mark).

render_element(Record) ->
    wf_tags:emit_tag(mark, Record#mark.body, [
        {class, ["mark", Record#mark.class]},
        {style, Record#mark.style}
    ]).
