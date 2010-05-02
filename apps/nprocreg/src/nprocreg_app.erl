% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(nprocreg_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    start(ignored, []).

start(_Type, StartArgs) ->
    case nprocreg_sup:start_link(StartArgs) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.
