% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_image).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, image).

render(ControlID, Record) -> 
	wf_tags:emit_tag(img, [
		{id, ControlID},
		{class, [image, Record#image.class]},
		{style, Record#image.style},
		{src, Record#image.image}
	]).