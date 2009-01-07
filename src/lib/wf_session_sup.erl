% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% Contributions from Dave Peticolas
% See MIT-LICENSE for licensing information.

-module(wf_session_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_session/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE). 


start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% For use by wf_session_server only. Starts one session.
start_session() ->
	supervisor:start_child(?SERVER, []).


init([]) ->
	RestartStrategy = simple_one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,
	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	ASession = {session, {'wf_session', start_link, []}, temporary, 2000, worker, [wf_session]},
	{ok, {SupFlags, [ASession]}}.
