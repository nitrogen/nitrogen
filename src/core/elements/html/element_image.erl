% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_image).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, image).

render(ControlID, Record) ->
	Attributes = [
		{id, ControlID},
		{class, [image, Record#image.class]},
		{style, Record#image.style},
		{src, Record#image.image}
	],

	FinalAttributes = case Record#image.alt of
		undefined -> Attributes;
		ImageAlt -> [{alt, ImageAlt}|Attributes] 
	end,

	wf_tags:emit_tag(img, FinalAttributes).