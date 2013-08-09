%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
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
    application:start(crypto),
    application:start(nprocreg),

    %% Start up Webmachine...
    application:load(webmachine),
    {ok, BindAddress} = application:get_env(webmachine, bind_address),
    {ok, Port} = application:get_env(webmachine, port),
    {ok, DocRoot} = application:get_env(webmachine, document_root),
    {ok, StaticPaths} = application:get_env(webmachine, static_paths),

    io:format("Starting Webmachine Server on ~s:~p~n", [BindAddress, Port]),

    Options = [
        {ip, BindAddress}, 
        {port, Port},
        {dispatch, dispatch(DocRoot, StaticPaths)}
    ],
    Web = {webmachine_mochiweb,
            {webmachine_mochiweb, start, [Options]},
            permanent, 5000, worker, [mochiweb_socket_server]},
    Processes = [Web],
    application:start(mochiweb),
    application:start(webmachine),
    {ok, { {one_for_one, 5, 10}, Processes} }.

dispatch(DocRoot, StaticPaths) -> 
    StaticDispatches = [make_static_dispatch(DocRoot, StaticPath) || StaticPath <- StaticPaths],
    StaticDispatches ++ [
        %% Static content handlers can be defined manually like so:
        %% {["css", '*'], static_resource, [{root, "./site/static/css"}]},
        %% {["images", '*'], static_resource, [{root, "./site/static/images"}]},
        %% {["nitrogen", '*'], static_resource, [{root, "./site/static/nitrogen"}]},
        %%
        %% But instead of doing it manually, we'll load it from the configuration

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

join_path(Root,Path) when is_binary(Root) orelse is_binary(Path) ->
    join_path(wf:to_list(Root),wf:to_list(Path));
join_path(Root,Path) ->
    RootEndsWithSlash = lists:last(Root)==$/,
    PathStartsWithSlash = hd(Path)==$/,
    if
        RootEndsWithSlash andalso PathStartsWithSlash -> 
            Root ++ tl(Path);
        not(RootEndsWithSlash) andalso not(PathStartsWithSlash) ->
            Root ++ "/" ++ Path
    end.

make_static_dispatch(DocRoot, StaticPath) ->
    TokenStatic = string:tokens(StaticPath,"/"),
    FormattedPath = TokenStatic ++ ['*'],
    {FormattedPath, static_resource, [{root, join_path(DocRoot,StaticPath)}]}.
