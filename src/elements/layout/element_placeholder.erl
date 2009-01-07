% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_placeholder).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, placeholder).

render(_ControlID, Record) -> wf:render(Record#placeholder.body).
