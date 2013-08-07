#!/usr/bin/env escript
% vim:ts=4 sw=4 et ft=erlang

main([File]) ->
    Dir = case file:consult(File) of
        {ok, Rebar} ->
            DepsDirs = proplists:get_value(deps_dir, Rebar, ["deps"]),
            hd(DepsDirs);
        {error, _} -> 
            "deps"
    end,
    io:format("~s~n",[Dir]).
