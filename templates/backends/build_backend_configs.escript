#!/usr/bin/env escript
%% vim: ts=4 sw=4 et ft=erlang

main([]) ->
    Backends = [
        "cowboy",
        "inets",
        "mochiweb",
        "webmachine",
        "yaws"
    ],
    [do_backend(B) || B <- Backends].

do_backend(Backend) ->
    RebarConfig = "rebar." ++ Backend ++ ".config",
    RebarConfigScript = RebarConfig ++ ".script",
    io:format("Generating ~s and ~s~n", [RebarConfig, RebarConfigScript]),

    DepsFile = "sources/" ++ Backend ++ ".deps",
    DepsScriptFile = DepsFile ++ ".script",
    LoadersFile = "sources/" ++ Backend ++ ".loaders",
    
    DepsComment = "%% BACKEND_DEPS",
    LoadersComment = "%% BACKEND_LOADERS",
    DepsScriptComment = "%% BACKEND_SCRIPT",

    replace("sources/rebar.base.config", RebarConfig, DepsComment, DepsFile),
    replace(RebarConfig, RebarConfig, LoadersComment, LoadersFile),
    replace("sources/rebar.base.config.script", RebarConfigScript, DepsScriptComment, DepsScriptFile).


replace(FromFile, ToFile, Comment, ContentsFile) ->
    {ok, FileBin} = file:read_file(FromFile),
    {ok, ContentsBin} = file:read_file(ContentsFile),
    
    FileStr = chomp(binary_to_list(FileBin)),
    ContentsStr = chomp(binary_to_list(ContentsBin)),

    NewStr = string:replace(FileStr, Comment, ContentsStr),

    ok = file:write_file(ToFile, NewStr).

chomp(Str) ->
    string:trim(Str, trailing, "\n").
