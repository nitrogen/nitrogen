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

get_items(download) -> [
    {download, "Download", "/download"}
];

get_items(demos) -> [
    {demos, "Demos", "/demos"}
];

get_items(learn) -> [
    {introduction, "Introduction", "/learn" },
    {news, "Latest News", "/web/discuss/news" },
    {documentation, "Documentation", "/documentation"},
    {screencasts, "Screencasts", "/web/learn/screencasts" },
    {about, "About Nitrogen", "/web/learn/about" }
];


get_items(involve) -> [
    {community, "Community", "/web/discuss/community"},
    {contribute, "Contribute", "/web/contribute"}    
];

get_items(_) -> [].



