#!/usr/bin/env escript
%% -*- erlang -*-
%% vim: ts=4 sw=4 et ft=erlang
%%
%% Coyright 2013 - Jesse Gumm
%% MIT License
%%
%%
%% add_overlay.escript base.config overlay.config out.config
%%
%% Takes the file overlay.config, and *appends* any overlay section of it to
%% the overlay section of base.config, and writes the changes to out.config

usage() ->
    io:format("Usage: ./add_overlay.escript out.config base.config [overlay1.config, overlay2.config, ...]~n").

main(Args) when length(Args) < 3 ->
    usage();
main([OutFile,BaseFile|OverlayFiles]) ->
    io:format("Generating ~p with merged overlays~n",[OutFile]),
    io:format("...Loading base file: ~p~n",[BaseFile]),

    {ok,BaseTerms} = file:consult(BaseFile),
    BaseOverlay = proplists:get_value(overlay, BaseTerms, []),

    NewOverlay = BaseOverlay ++ load_overlays(OverlayFiles),

    NewTerms = proplists:delete(overlay, BaseTerms) ++ [{overlay, NewOverlay}],

    FinalTerms = [io_lib:format("~p.~n",[T]) || T <- NewTerms],

    io:format("...Writing ~p~n",[OutFile]),
    file:write_file(OutFile, FinalTerms),
    io:format("...SUCCESS~n").


load_overlays([]) ->
    [];
load_overlays([File | Rest]) ->
    io:format("...Merging Overlay from ~p~n",[File]),
    {ok, Terms} = file:consult(File),
    Overlay = proplists:get_value(overlay, Terms, []),
    Overlay ++ load_overlays(Rest).
