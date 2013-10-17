#!/usr/bin/env escript
%% vim: ts=4 sw=4 et ft=erlang

main([SrcFile, DepsFile, DestFile]) ->
    io:format("Generating ~p~n", [DestFile]),
    {ok, Src} = file:read_file(SrcFile),
    {ok, Deps} = file:read_file(DepsFile),

    Regex = "%% START_PLATFORM_DEPENDENCIES.*END_PLATFORM_DEPENDENCIES",

    [Part1, Part2] = re:split(Src, Regex, [{return, binary},dotall]),

    NewContents = [Part1,"\n", Deps, "\n", Part2],
    ok = file:write_file(DestFile, NewContents).

