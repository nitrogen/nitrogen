% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_radio).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, radio).

render(ControlID, Record) -> 
	% Set the group to the current ControlID...

	F = fun(X) ->
		case is_record(X, radioitem) of
			true -> X#radioitem { name=ControlID };
			false -> X
		end
	end,
	Body = [F(X) || X <- Record#radio.body],
	
	% Render the record...
	element_panel:render(ControlID, #panel {
		class="radio " ++ wf:to_list(Record#radio.class),
		style=Record#radio.style,
		body=Body
	}).