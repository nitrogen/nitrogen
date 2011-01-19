%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(nitrogen_webmachine).
-export([
    init/1, 
    to_html/2, 
    allowed_methods/2,
    post_is_create/2,
    process_post/2
]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").


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
