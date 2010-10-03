% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_footer).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, footer).

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
    Y = lists:any(CheckHeaderOrFooter, Record#footer.body),
    if
        Y ->
            "<b style=\"color: red;\">footer cannot have another header or footer as child element</b>";
        true ->
            wf_tags:emit_tag(footer, Record#footer.body, [
                {class, ["footer", Record#footer.class]},
                {style, Record#footer.style}
            ])
    end.
