#!/usr/bin/env escript
%% -*- erlang -*-
%% vim: ts=4 sw=4 et ft=erlang
%%
%% Coyright 2012 - Jesse Gumm
%% MIT License
%%
%%
%% This merely takes a normal reltool.config file and add options to make it
%% a slim release for Nitrogen

main([File]) ->
    main([File,File]);
main([FromFile,ToFile]) ->
    {ok,Terms} = file:consult(FromFile),

    Sys = proplists:get_value(sys, Terms),
    Sys2 = [{profile, development}, {excl_lib,otp_root} | proplists:delete(profile, Sys)],
    SysFinal = Sys2,

    Overlay = proplists:get_value(overlay, Terms),
    Overlay2 = lists:delete({copy,"./overlay/erts/*","{{erts_vsn}}/bin"},Overlay),
    Overlay3 = [{copy,"./overlay/erts/nodetool","releases/{{rel_vsn}}/nodetool"} | Overlay2],
    OverlayFinal = Overlay3,

    Terms2 = proplists:delete(sys, Terms),
    Terms3 = proplists:delete(overlay, Terms2),
    Terms4 = [{sys, SysFinal}, {overlay, OverlayFinal} | Terms3],

    FinalTerms = [io_lib:format("~p.~n",[T]) || T <- Terms4],

    if
        FromFile=:=ToFile ->
            io:format("Turning ~p into a slim release~n", [FromFile]);
        true ->
            io:format("Making ~p a slim release as ~p~n", [FromFile, ToFile])
    end,
    file:write_file(ToFile, FinalTerms).

