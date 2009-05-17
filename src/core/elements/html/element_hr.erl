% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_hr).
-compile(export_all).
-include ("wf.inc").

reflect() -> record_info(fields, hr).

render(ControlID, Record) -> 
	wf_tags:emit_tag(hr, [
		{size, 1},
		{id, ControlID},
		{class, [hr, Record#hr.class]},
		{style, Record#hr.style}
	]).