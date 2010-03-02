% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_radiogroup).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, radiogroup).

render_element(Record) -> 
    % Set the group to the current HtmlID...
    Anchor = Record#radiogroup.anchor,
    Body = apply_name(Anchor, Record#radiogroup.body),

    % Render the record...
    element_panel:render_element(#panel {
        id=Record#radiogroup.id,
        anchor=Record#radiogroup.anchor,
        class=[radiogroup, Record#radiogroup.class],
        style=Record#radiogroup.style,
        body=Body
    }).

apply_name(Name, Terms) ->
    [do_apply(Name, X) || X <- Terms].

do_apply(Name, X) when is_record(X, radio) ->
    X#radio {name = Name};
do_apply(Name, X) when is_record(X, bind) ->
    Body2 = apply_name(Name, X#bind.body),
    X#bind{body = Body2};
do_apply(_Name, X) ->
    X.
