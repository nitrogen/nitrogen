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
        "_build/default/lib/nitrogen_core/priv/www",
        %% account for the possibility this is a mix umbrella app;
        %% dependencies are in <base>/deps/.  mix will execute rebar3
        %% from the nitrogen base directory however, so the relative
        %% path works here.
        "../../deps/nitrogen_core/www"
    ],
    Src = find_first_loc(PossibleLocations),
    Dest = "priv/static/nitrogen",

    io:format("Removing existing assets directory ~s~n", [Dest]),
    ok = case file:del_dir_r(Dest) of
        ok -> ok;
        {error, enoent} -> ok;
        Err -> Err
    end,

    case Action of
        "copy" ->
            io:format("Creating assets directory ~s~n", [Dest]),
            ok = filelib:ensure_path(Dest),
            rcopy(filelib:wildcard("**", Src), Src, Dest);
        "link" ->
            {ok, Cwd} = file:get_cwd(),
            AbsSrc = filename:join(Cwd, Src),
            io:format("Linking assets to ~s~n", [Dest]),
            ok = file:make_symlink(AbsSrc, Dest)
    end,

    %% in a mix umbrella app, we want all the nitro priv assets in
    %% <base>/priv.  in a more perfect world, all of the assets might
    %% be in a single subdirectory (thus avoiding the possibility of
    %% clobbering existing <base>/priv files); that wouldn't jive with
    %% how nitro finds its stuff though and changing that would negate
    %% backwards compatibility.
    case string:find(Src, "../../deps") of
        nomatch -> ok;
        _ ->
            io:format("Copying assets to umbrella base privdir.~n"),
            BasePriv = "../../priv",
            NitroPriv = "priv",
            ok = filelib:ensure_path(BasePriv),
            rcopy(filelib:wildcard("**", NitroPriv), NitroPriv, BasePriv)
    end.

rcopy([], _SourceDir, _DestDir) ->
    ok;
rcopy([F|T], SourceDir, DestDir) ->
    S = filename:join(SourceDir, F),
    D = filename:join(DestDir, F),
    case filelib:is_dir(S) of
        true  -> file:make_dir(D);
        false -> file:copy(S, D)
    end,
    rcopy(T, SourceDir, DestDir).


find_first_loc([]) ->
    exit("nitrogen_core directory not found to link the www directory");
find_first_loc([H|T]) ->
    case filelib:is_dir(H) of
        true -> H;
        false -> find_first_loc(T)
    end.
