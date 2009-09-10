% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_radiogroup).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, radiogroup).

render_element(HtmlID, Record) -> 
	% Set the group to the current HtmlID...
	Body = apply_name(HtmlID, Record#radiogroup.body),
	
	% Render the record...
	element_panel:render_element(HtmlID, #panel {
		id=Record#radiogroup.id,
		class="radiogroup " ++ wf:to_list(Record#radiogroup.class),
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
