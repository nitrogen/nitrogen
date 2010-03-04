-module (topmenu1).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

render(Item) ->
	Items = [
		{home, "Home", "/"},
		{learn, "Learn", "/web/learn"},
		{discuss, "Discuss", "/web/discuss/news"},
		{reference, "Reference", "/web/reference"},
		{download, "Code", "/web/download"}
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
