-module(mprocreg_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    start(ignored, []).

start(_Type, StartArgs) ->
    case mprocreg_sup:start_link(StartArgs) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.
