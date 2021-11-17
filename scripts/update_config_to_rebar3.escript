#!/usr/bin/env escript
%% -*- erlang -*-
%% vim: ts=4 sw=4 et ft=erlang
%%
%% Coyright 2021 - Jesse Gumm
%% MIT License
%%
%%
%% Usage: ./update_config_to_rebar3.escript [rebar.config]
%%
%% Takes the target rebar.config, analyzes the deps, and adds a relx item to it

main([]) ->
    main(["rebar.config"]);
main([File]) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    Config = case file:consult(File) of
        {error, Reason} ->
            io:format("FAIL! Reason: ~p~n",[Reason]),
            halt(1);
        {ok, X} ->
            X
    end,
    C2 = remove_option(sub_dirs, Config),
    C3 = remove_option(deps_dir, C2),
    C4 = remove_sub_option(erl_opts, i, C3),
    C5 = add_plugin(rebar3_run, C4),
    C6 = add_relx(C5),
    C7 = hexify_deps(C6),
    FinalConfig = C7,
    case FinalConfig==Config of
        true ->
            io:format("No changes were made to ~s~n",[File]);
        false ->
            NoDeps = lists:keydelete(deps, 1, FinalConfig),
            Formatted = writable_terms(NoDeps),
            OnlyDeps = writable_deps(proplists:get_value(deps, FinalConfig, [])),
            NewBody = [Formatted, OnlyDeps],
            io:format("Writing new ~s~n",[File]),
            file:write_file(File, NewBody)
    end.

add_plugin(Plugin, Config) ->
    io:format("Adding plugin ~p: ",[Plugin]),
    Plugins = proplists:get_value(plugins, Config, []),
    case lists:member(Plugin, Plugins) of
        true ->
            io:format("already present, skipping~n"),
            Config;
        false ->
            NewPlugins = [Plugin | Plugins],
            C2 = lists:keydelete(plugins, 1, Config),
            io:format("added~n"),
            [{plugins, NewPlugins} | C2]
    end.

writable_terms(Terms) ->
    lists:map(fun(Term) ->
        io_lib:format("~p.~n~n", [Term])
    end, Terms).

writable_deps([]) ->
    [];
writable_deps(Deps) ->
    NumSpacesAfterName = longest_dep_name(Deps) + 2,
    Formatted = lists:map(fun(Dep) ->
        format_dep(NumSpacesAfterName, Dep)
    end, Deps),
    "{deps, [\n" ++ lists:join(",\n", Formatted) ++ "\n]}.".

format_dep(_, Dep) when is_atom(Dep) ->
    "\t" ++ atom_to_list(Dep);
format_dep(NumSpacesAfterNameBase, {Name, Source}) when is_tuple(Source) ->
    NumSpacesAfterName = NumSpacesAfterNameBase - length(atom_to_list(Name)),
    Spaces = lists:duplicate(NumSpacesAfterName, 32), %% 32 = space ASCII
    "\t{" ++ atom_to_list(Name) ++ "," ++ Spaces ++ format_source(Source) ++ "}".
    


format_source({VCS, URL, Tag}) ->
    "{" ++ atom_to_list(VCS) ++ ",\t\"" ++ URL ++ "\",\t" ++ io_lib:format("~p", [Tag]) ++ "}";
format_source({VCS, URL}) ->
    "{" ++ atom_to_list(VCS) ++ ",\t\"" ++ URL ++ "\"}".

longest_dep_name(Deps) ->
    lists:foldl(fun
        (Dep, Acc) when is_atom(Dep) ->
            L = length(atom_to_list(Dep)),
            lists:max([L, Acc]);
        (Dep, Acc) when is_tuple(Dep) ->
            App = element(1, Dep),
            L = length(atom_to_list(App)),
            lists:max([L, Acc])
    end, 4, Deps).


remove_sub_option(Rule, Subrule, Config) ->
    io:format("Removing ~p/~p rule (if it exists): ", [Rule, Subrule]),
    case lists:keyfind(Rule, 1, Config) of
        false ->
            io:format("~p does not exist, skipping.~n",[Rule]),
            Config;
        {Rule, Opts} ->
            case lists:keyfind(Subrule, 1, Opts) of
                false ->
                    io:format("NO, ~p exists, but no ~p option inside, skipping.~n",[Rule, Subrule]),
                    Config;
                {Subrule, _} ->
                    io:format("YES, exists, removing~n"),
                    NewOpts = lists:keydelete(Subrule, 1, Opts),
                    NewConfig = lists:keydelete(Rule, 1, Config),
                    NewRule = {Rule, NewOpts},
                    [NewRule | NewConfig]
            end
    end.


remove_option(Rule, Config) ->
    io:format("Removing ~p rule (if it exists): ", [Rule]),
    case lists:keyfind(Rule, 1, Config) of
        false ->
            io:format("NO, does not exist, skipping.~n"),
            Config;
        _ ->
            io:format("YES, exists, removing.~n"),
            lists:keydelete(Rule, 1, Config)
    end.

add_relx(Config) ->
    io:format("Adding Relx config: "),
    Relx = proplists:get_value(relx, Config, undefined),
    case Relx of
        undefined ->
            C2 = add_relx_inner(Config),
            io:format("added.~n"),
            C2;
        _ ->
            io:format("already present, skipping.~n"),
            Config
    end.

add_relx_inner(Config) ->
    Deps = proplists:get_value(deps, Config, []),
    
    Apps = lists:map(fun
        (App) when is_atom(App) ->
            App;
        (App) when is_tuple(App) ->
            element(1, App)
    end, Deps),

    Applines = lists:map(fun(App) ->
        {App, load}
    end, Apps),

    NewRelx = {relx, [
        {release, {nitrogen, "0.0.1"}, [sasl] ++ Applines ++ [nitrogen]},
        {sys_config, "etc/autogenerated.config.all"},
        {vm_args, "etc/vm.args"},
        {dev_mode, true},
        {mode, dev},
        {include_erts, false},
        {extended_start_script, true},
        {overlay, [
            {copy, "site", "site"},
            {copy, "priv", "priv"}
        ]}
    ]},
    Config ++ [NewRelx].

can_ignore_dep(Dep) ->
    lists:member(Dep, [simple_bridge, nitro_cache, nprocreg, rekt, qdate]).

hexify_deps(Config) ->
    io:format("Hexifying Dependancies...~n"),
    Deps = proplists:get_value(deps, Config, []),
    NewDeps = lists:map(fun
        (App) when is_atom(App) ->
            case can_ignore_dep(App) of
                true ->
                    io:format("...~p does not need to be explicitly specified as a dependency, removing.~n",[App]),
                    undefined;
                false ->
                    io:format("...~p is already Hexified~n", [App]),
                    App
            end;
        (App) when is_tuple(App) ->
            Appname = element(1, App),
            case can_ignore_dep(Appname) of
                true ->
                    io:format("...~p does not need to be explicitly specified as a dependency, removing.~n",[App]),
                    undefined;
                false ->
                    io:format("...~p? ", [Appname]),
                    case is_in_hex(Appname) of
                        true ->
                            io:format("YES.~n"),
                            Appname;
                        false ->
                            io:format("NO.~n"),
                            App
                    end
            end
    end, Deps),
    NewDeps2 = [X || X <- NewDeps, X=/=undefined],
    lists:keydelete(deps, 1, Config) ++ [{deps, NewDeps2}].

is_in_hex(App) ->
    BaseURL = "https://hex.pm/api/packages/",
    URL = BaseURL ++ atom_to_list(App),
    Headers = [{"user-agent", "Erlang's httpc, baby!"}],
    Request = {URL, Headers},
    case httpc:request(get, Request, [], []) of
        {ok, {{_, 200, "OK"}, _, _}} -> true;
        _ -> false
    end.
