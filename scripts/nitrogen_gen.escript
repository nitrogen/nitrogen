#!/usr/bin/env escript

-export([main/1]).

%% External API

main([Name]) ->
    main([Name, "inets"]);
main([Name, Type]) ->
    ensure(),
    DestDir = filename:absname(Name),
    ok = nitrogen_skel:create_project(DestDir, Type);

main(_) ->
    usage().

%% Internal API

ensure() ->
    code:add_patha(filename:join(filename:dirname(escript:script_name()),
                                 "../ebin")).

usage() ->
    io:format("usage: ~s name [inets|mochiweb|yaws]~n",
              [filename:basename(escript:script_name())]),
    halt(1).

