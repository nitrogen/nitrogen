-module (leftmenu).
-include_lib ("nitrogen/include/wf.hrl").
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
	
get_items(home) -> [
];
	
get_items(learn) -> [
	{ learn, "Introduction", "/web/learn" },
	{ screencasts, "Screencasts", "/web/learn/screencasts" },
	{ samples, "Code Samples", "/web/samples" },
	{ about, "About Nitrogen", "/web/learn/about" }
];
	
get_items(discuss) -> [
	{ news, "Latest News", "/web/discuss/news" },
	{ community, "Community", "/web/discuss/community" }
];

get_items(reference) -> [
	{ overview, "Overview", "/web/reference" },
	{ elements, "Elements", "/web/reference/elements" },
	{ actions, "Actions", "/web/reference/actions" },
	{ validators, "Validators", "/web/reference/validators" },
	{ api, "API", "/web/reference/api" },
	{ wiki, "Wiki (GitHub)", "http://github.com/rklophaus/nitrogen/wikis" }
];
	
get_items(code) -> [
	{ download, "Download", "/web/download" },
	{ contribute, "Contribute", "/web/contribute" }
];

get_items(_) -> [].
	
	

