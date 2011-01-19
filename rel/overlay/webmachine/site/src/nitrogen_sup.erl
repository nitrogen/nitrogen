%% -*- mode: nitrogen -*-
-module(nitrogen_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1,
    dispatch/0
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

    %% Start up Webmachine...
    application:load(webmachine),
    {ok, BindAddress} = application:get_env(webmachine, bind_address),
    {ok, Port} = application:get_env(webmachine, port),

    io:format("Starting Webmachine Server on ~s:~p~n", [BindAddress, Port]),

    Options = [
        {ip, BindAddress}, 
        {port, Port},
        {dispatch, dispatch()}
    ],
    webmachine_mochiweb:start(Options),

    {ok, { {one_for_one, 5, 10}, []} }.

dispatch() -> 
    [
        %% Static content handlers...
        {["css", '*'], static_resource, [{root, "./site/static/css"}]},
        {["images", '*'], static_resource, [{root, "./site/static/images"}]},
        {["nitrogen", '*'], static_resource, [{root, "./site/static/nitrogen"}]},

        %% Add routes to your modules here. The last entry makes the
        %% system use the dynamic_route_handler, which determines the
        %% module name based on the path. It's a good way to get
        %% started, but you'll likely want to remove it after you have
        %% added a few routes.
        %%
        %% p.s. - Remember that you will need to RESTART THE VM for
        %%        dispatch changes to take effect!!!
        %% 
        %% {["path","to","module1",'*'], nitrogen_webmachine, module_name_1}
        %% {["path","to","module2",'*'], nitrogen_webmachine, module_name_2}
        %% {["path","to","module3",'*'], nitrogen_webmachine, module_name_3}
        {['*'], nitrogen_webmachine, dynamic_route_handler}
    ].



