% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_radiogroup).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, radiogroup).

render(ControlID, Record) -> 
	% Set the group to the current ControlID...

	F = fun(X) ->
		case is_record(X, radio) of
			true -> X#radio { name=ControlID };
			false -> X
		end
	end,
	Body = [F(X) || X <- Record#radiogroup.body],
	
	% Render the record...
	element_panel:render(ControlID, #panel {
		class="radiogroup " ++ wf:to_list(Record#radiogroup.class),
		style=Record#radiogroup.style,
		body=Body
	}).