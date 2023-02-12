#!/usr/bin/env escript
%% -*- erlang -*-
%% vim: ts=4 sw=4 et ft=erlang
%%
%% Coyright 2022 - Jesse Gumm
%%
%% MIT License
%%
%% About
%%
%% This will copy or link the nitrogen static directory into the
%% priv/static/nitrogen directory based on where it finds the nitrogen_core
%% directory (checkouts or lib)

main([]) ->
    main(["copy"]);
main([Action]) when Action=="copy"; Action=="link" ->
    PossibleLocations = [
        "_checkouts/nitrogen_core/www", %% old loc, but let's check
        "_checkouts/nitrogen_core/priv/www",
        "_build/default/checkouts/nitrogen_core/priv/www",
        "_build/default/lib/nitrogen_core/priv/www"
    ],
    Src = find_first_loc(PossibleLocations),
    Dest = "priv/static/nitrogen",
  
    %% These need to be changed such that they don't rely on linux tools and just use
    %% file:copy and file:make_symlink
    cmd("rm -fr " ++ Dest),
    case Action of
        "copy" ->
            cmd("mkdir -p " ++ Dest),
            cmd("cp -r " ++ Src ++ "/* " ++ Dest);
        "link" ->
            cmd("ln -s `pwd`/" ++ Src ++ " " ++ Dest)
    end.
        

cmd(Cmd) ->
    io:format("Running: ~s~n", [Cmd]),
    os:cmd(Cmd).


find_first_loc([]) ->
    exit("nitrogen_core directory not found to link the www directory");
find_first_loc([H|T]) ->
    case filelib:is_dir(H) of
        true -> H;
        false -> find_first_loc(T)
    end.
    
