-module (topmenu1).
-include ("wf.inc").
-compile(export_all).

render(Item) ->
	Items = [
		{learn, "Samples", "/web/samples"},
		{reference, "Reference", "/web/reference"}
	],
	
	Transform = fun(DataRow, Acc) ->
		case element(1, DataRow) == Item of
			true -> {DataRow, Acc, {link@class, selected}};
			false -> {DataRow, Acc, []}
		end
	end,
	
	% Response.
	#list { body=#bind {
		data=Items,
		map={ ignore, link@text, link@url },
		transform=Transform,
		body=#listitem { body=#link { id=link }}
	}}.