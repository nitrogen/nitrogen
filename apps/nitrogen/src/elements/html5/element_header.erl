% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_header).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, header).

render_element(Record) ->
    CheckHeaderOrFooter =   fun (X) ->
                                if
                                    is_record(X, header) ->
                                        true;
                                    is_record(X, footer) ->
                                        true;
                                    true ->
                                        false
                                end
                            end,
    Y = lists:any(CheckHeaderOrFooter, Record#header.body),
    if
        Y ->
            "<b style=\"color: red;\">header cannot have another header or footer as child element</b>";
        true ->
            wf_tags:emit_tag(header, Record#header.body, [
                {class, ["header", Record#header.class]},
                {style, Record#header.style}
            ])
    end.
