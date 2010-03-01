-module(mprocreg_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(_) ->
    MProcReg = {mprocreg, {mprocreg, start_link, []}, permanent, 2000, worker, [mprocreg]},
    {ok,{{one_for_one, 15, 60}, [MProcReg]}}.
