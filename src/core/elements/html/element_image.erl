% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_image).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, image).

render_element(HtmlID, Record, Context) ->
	Attributes = [
		{id, HtmlID},
		{class, [image, Record#image.class]},
		{style, Record#image.style},
		{src, Record#image.image}
	],

	FinalAttributes = case Record#image.alt of
		undefined -> Attributes;
		ImageAlt -> [{alt, ImageAlt}|Attributes] 
	end,

	Elements = wf_tags:emit_tag(img, FinalAttributes),
	{ok, Elements, Context}.