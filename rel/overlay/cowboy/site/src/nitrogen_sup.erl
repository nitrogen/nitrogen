%% -*- mode: nitrogen -*-
-module(nitrogen_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% Start the Process Registry...
    application:start(nprocreg),

    %% Start Cowboy...
    application:start(cowboy),
    {ok, BindAddress} = application:get_env(cowboy, bind_address),
    {ok, Port} = application:get_env(cowboy, port),
    {ok, ServerName} = application:get_env(cowboy, server_name),
    {ok, DocRoot} = application:get_env(cowboy, document_root),

    io:format("Starting Cowboy Server (~s) on ~s:~p, root: '~s'~n",
              [ServerName, BindAddress, Port, DocRoot]),

    %% Start Cowboy...
    Dispatch = [{'_', [{'_', nitrogen_cowboy, []}]}],
    HttpOpts = [{max_keepalive, 50}, {dispatch, Dispatch}],
	cowboy:start_listener(http, 100,
                          cowboy_tcp_transport, [{port, Port}],
                          cowboy_http_protocol, HttpOpts),

    {ok, { {one_for_one, 5, 10}, []} }.

