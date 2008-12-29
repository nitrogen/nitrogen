% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_list).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, list).

render(ControlID, Record) -> 
	Tag = case Record#list.numbered of 
		true -> "ol";
		_ -> "ul"
	end,

	[
		wf:f("<~s id='~s' class='list ~s' style='~s'>", [
			Tag,
			ControlID,
			Record#list.class,
			Record#list.style
		]),
		wf:render(Record#list.body),
		wf:f("</~s>", [Tag])
	].