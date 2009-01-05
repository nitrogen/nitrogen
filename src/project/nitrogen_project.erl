-module(nitrogen_project).
-export([create_page/2, replace_content/2]).

create_page(Url, Dir) ->
    DestFile = dest_file(Dir, Url),
    io:format("Creating module ~p ~n", [DestFile]),
    copy_module( src_file("PAGE.erl"), DestFile ).

replace_content([], Contents) ->
    Contents;
replace_content([{Key, Value}|Rest], Contents) ->
    {ok, Replaced, _} = regexp:gsub(Contents, Key, Value),
    replace_content(Rest, Replaced).

%% Internal Functions
src_file(FileName) ->
    ModuleDir = filename:dirname(code:which(?MODULE)),
    filename:join([ModuleDir, "../", "priv", "skel", FileName]).

dest_file(Dir, Url) ->
    Tokens = string:tokens(Url, "/"),
    Filename = string:join(Tokens, "_"),
    filename:join(Dir, Filename++".erl").

copy_module(SrcFile, DestFile) ->
    Module = filename:basename(DestFile, ".erl"),
    {ok, Mode} = file:read_file_info(SrcFile),
    {ok, Bin} = file:read_file(SrcFile),
    Changes =  [{"MODULE_NAME", Module}],
    Replaced = replace_content(Changes, binary_to_list(Bin)),
    ok = file:write_file(DestFile, list_to_binary(Replaced)),
    file:write_file_info(DestFile, Mode).
    
