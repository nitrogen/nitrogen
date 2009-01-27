-module (leftmenu).
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
	
get_items(reference) -> [
	{ overview, "Overview", "/web/reference" },
	{ elements, "Elements", "/web/reference/elements" },
	{ actions, "Actions", "/web/reference/actions" },
	{ validators, "Validators", "/web/reference/validators" },
	{ api, "API", "/web/reference/api" },
	{ wiki, "Wiki (GitHub)", "http://github.com/rklophaus/nitrogen/wikis" }
];

get_items(_) -> [].
	
	

