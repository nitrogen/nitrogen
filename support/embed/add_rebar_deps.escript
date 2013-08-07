#!/usr/bin/env escript
% vim: ts=4 sw=4 et ft=erlang

main([FromFile, ToFile]) ->
    io:format("Adding deps from ~p into ~p~n",[FromFile, ToFile]),
    {ok, ServerFile} = file:consult(FromFile),
    case file:consult(ToFile) of
        {error, _} ->
            file:copy(FromFile, ToFile);
        {ok, TargetRebar} ->
            file:rename(ToFile, ToFile ++ ".orig"),
            FromDeps = proplists:get_value(deps, ServerFile),
            ToDeps = proplists:get_value(deps, TargetRebar, []),
            NewToDeps = only_missing_deps(FromDeps, ToDeps) ++ ToDeps,
            NewTargetRebar = replace_rule(deps, NewToDeps, TargetRebar),
            FinalTerms = [io_lib:format("~p.~n",[T]) || T <- NewTargetRebar],
            file:write_file(ToFile, FinalTerms)
    end.


only_missing_deps([], _ToDeps) ->
    [];
only_missing_deps([FromDep|FromDeps], ToDeps) ->
    App = element(1, FromDep),
    case lists:keyfind(App, 1, ToDeps) of
        false -> [FromDep | only_missing_deps(FromDeps, ToDeps)];
        _ -> only_missing_deps(FromDeps, ToDeps)
    end.

replace_rule(RuleKey, RuleVal, [{RuleKey, _} | T]) ->
    [{RuleKey, RuleVal} | T];
replace_rule(RuleKey, RuleVal, [H|T]) ->
    [H | replace_rule(RuleKey, RuleVal, T)].

