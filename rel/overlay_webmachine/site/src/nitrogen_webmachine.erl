%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(nitrogen_webmachine).
-export([
    start/2,
    init/1, 
    to_html/2, 
    allowed_methods/2,
    post_is_create/2,
    process_post/2
]).

-include_lib("nitrogen/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").

start(_, _) ->
    % Read config...
    {ok, BindAddress} = application:get_env(webmachine, bind_address),
    {ok, Port} = application:get_env(webmachine, port),
    io:format("Starting Webmachine Server on ~s:~p~n", [BindAddress, Port]),

    % Start Mochiweb...
    Options = [
        {ip, BindAddress}, 
        {port, Port},
        {dispatch, dispatch()}
    ],
    webmachine_mochiweb:start(Options).

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
        %% {["path","to","module1",'*'], ?MODULE, module_name_1}
        %% {["path","to","module2",'*'], ?MODULE, module_name_2}
        %% {["path","to","module3",'*'], ?MODULE, module_name_3}
        {['*'], ?MODULE, dynamic_route_handler}
    ].


%% Resource Functions %%

-record(state, {page_module}).

init(PageModule) -> 
    State = #state { page_module=PageModule },
    {ok, State}.

allowed_methods(ReqData, State) -> 
    {['HEAD', 'GET', 'POST'], ReqData, State}.

post_is_create(ReqData, State) -> 
    {false, ReqData, State}.

to_html(ReqData, State) ->
    PageModule = State#state.page_module,
    {ok, Data, ReqData1} = do_nitrogen(PageModule, ReqData),
    {Data, ReqData1, State}.

process_post(ReqData, State) ->
    PageModule = State#state.page_module,
    {ok, Data, ReqData1} = do_nitrogen(PageModule, ReqData),
    ReqData2 = wrq:set_resp_body(Data, ReqData1),
    {true, ReqData2, State}.

do_nitrogen(PageModule, Req) ->
    % Make request and response bridges...
    RequestBridge = simple_bridge:make_request(webmachine_request_bridge, Req),
    ResponseBridge = simple_bridge:make_response(webmachine_response_bridge, Req),
    nitrogen:init_request(RequestBridge, ResponseBridge),

    %% If we have been given a module name, use it, otherwise use the
    %% dynamic_route_handler.
    case PageModule of
        dynamic_route_handler -> 
            nitrogen:handler(dynamic_route_handler, []);
        _Other ->
            nitrogen:handler(static_route_handler, PageModule)
    end,

    nitrogen:run().
