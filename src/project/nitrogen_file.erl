% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% Module by Jon Gretar Borgthorsson
% See MIT-LICENSE for licensing information.
-module(nitrogen_file).
-include_lib("kernel/include/file.hrl").
-export([create_page/2, copy_file/2, copy_file/3]).

create_page(Url, Dir) ->
    SrcFile = nitrogen_project:src_path("priv/skel/PAGE.erl"),
    DestFile = url_to_file(Dir, Url),
    Changes =  [{"PAGE", filename:basename(DestFile, ".erl")}],
    copy_file( SrcFile, DestFile, Changes ).

copy_file(SrcFile, DestFile) ->
    io:format("Copy Nitrogen Static: ~p~n", [filename:basename(DestFile)]),
    {ok, Mode} = file:read_file_info(SrcFile),
    file:copy(SrcFile, DestFile),
    file:write_file_info(DestFile, Mode).

copy_file(SrcFile, DestFile, Changes) ->
    ParsedDestFile = replace_content(Changes, DestFile),
    io:format("Creating file: ~p~n", [filename:basename(ParsedDestFile)]),
    {ok, Mode} = file:read_file_info(SrcFile),
    {ok, Bin} = file:read_file(SrcFile),
    Contents = replace_content(Changes, binary_to_list(Bin)),
    ok = file:write_file(ParsedDestFile, list_to_binary(Contents)),
    file:write_file_info(ParsedDestFile, Mode).


%% Internal Functions
url_to_file(Dir, Url) ->
    Tokens = string:tokens(Url, "/"),
    Filename = string:join(Tokens, "_"),
    filename:join(Dir, Filename++".erl").

replace_content([], Contents) ->
    Contents;
replace_content([{Key, Value}|Rest], Contents) ->
    {ok, Replaced, _} = regexp:gsub(Contents, Key, Value),
    replace_content(Rest, Replaced).

    
