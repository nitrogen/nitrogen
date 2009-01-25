-module (topmenu2).
-include ("wf.inc").
-compile(export_all).

render(Group, Item) -> 
	Items = get_items(Group),
	Transform = fun(DataRow, Acc) ->
		case element(1, DataRow) == Item of
			true -> {DataRow, Acc, {link@class, selected}};
			false -> {DataRow, Acc, []}
		end
	end,
	
	% Response.
	#list { body=[
		#bind {
			data=Items,
			map={ ignore, link@text, link@url },
			transform=Transform,
			body=#listitem { body=#link { id=link }}
		}
	]}.
	
get_items(_) -> [].
	
	

