-module(nitrogen_project).
-export([create_page/2]).

create_page(Url, Dir) ->
    DestFile = dest_file(Dir, Url),
    io:format("Creating module ~p ~n", [DestFile]),
    copy_module( src_file("PAGE.erl"), DestFile ).

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
    {ok, B} = file:read_file(SrcFile),
    {ok, S, _} = regexp:gsub(binary_to_list(B), "MODULE_NAME", Module),
    ok = file:write_file(DestFile, list_to_binary(S)),
    file:write_file_info(DestFile, Mode).
    
