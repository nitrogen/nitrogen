% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(nprocreg_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(_) ->
    NProcReg = {nprocreg, {nprocreg, start_link, []}, permanent, 2000, worker, [nprocreg]},
    {ok,{{one_for_one, 15, 60}, [NProcReg]}}.
