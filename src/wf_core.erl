-module (wf_core).
-include ("wf.inc").
-export ([
	run/0
]).

% nitrogen_core - 
% --
% Render a single Nitrogen page or inline application. This can be called
% from other Erlang web frameworks or resource servers, such as WebMachine, 
% Erlang Web, ErlyWeb, etc.

run() ->
	Request = wf_context:request_bridge(),
	Response = wf_context:response_bridge(),
	try 
		case Request:error() of
			none -> run_catched();
			Other -> 
				Message = wf:f("Errors: ~p~n", [Other]),
				Response1 = Response:data(Message),
				Response1:build_response()
		end
	catch Type : Error -> 
		?LOG("~p~n", [{error, Type, Error, erlang:get_stacktrace()}])
	end.

run_catched() ->
	% Get the handlers from querystring, if they exist...
	deserialize_context(),
	
	% Initialize all handlers...
	call_init_on_handlers(),
	
	% Deserialize the event if available...
	wf_event:update_context_with_event(),

	% TODO - Check for access

	% Call the module...
	case wf_context:type() of
		first_request    -> 
			run_first_request(), 
			finish_dynamic_request();
		postback_request -> 
			run_postback_request(), 
			finish_dynamic_request();
		static_file      -> 
			finish_static_request()
	end.
	
finish_dynamic_request() ->
	% Update flash and render.
	element_flash:update(),	

	% Get elements and actions...
	Elements = wf_context:data(),
	wf_context:clear_data(),
	Actions = wf_context:actions(),
	wf_context:clear_actions(),
	
	% Render...
	{ok, Html, Javascript} = wf_render:render(Elements, Actions),

	% Call finish on all handlers.
	call_finish_on_handlers(),
	
	% Create Javascript to set the state...
	StateScript = serialize_context(),
	Javascript1 = [StateScript, Javascript],
	case wf_context:type() of
		first_request       -> build_first_response(Html, Javascript1);
		postback_request    -> build_postback_response(Javascript1)
	end.
	
finish_static_request() ->
	Path = wf_context:path_info(),
	build_static_file_response(Path).
	
%%% SERIALIZE AND DESERIALIZE STATE %%%

% serialize_context_state/0 -
% Serialize part of Context and send it to the browser
% as Javascript variables.
serialize_context() ->
	Page = wf_context:page_context(),
	Handlers = wf_context:handlers(),
	SerializedContextState = wf:pickle([Page, Handlers]),
	[
		wf_render_actions:generate_scope_script(),
		wf:f("Nitrogen.$set_param('pageContext', '~s');", [SerializedContextState])
	].

% deserialize_context_state/1 -
% Updates the context with values that were stored
% in the browser by serialize_context_state/1.
deserialize_context() ->
	RequestBridge = wf_context:request_bridge(),	
	Params = RequestBridge:post_params(),
	
	% Deserialize page_context and handler_list if available...
	SerializedPageContext = proplists:get_value("pageContext", Params),
	[Page, Handlers] = case SerializedPageContext of
		undefined -> [wf_context:page_context(), wf_context:handlers()];
		Other -> wf:depickle(Other)
	end,	
	
	% Deserialize dom_paths if available...
	DomPathList = proplists:get_value("domPaths", Params),
	DomPaths = wf_path:split_dom_paths(DomPathList),
	
	% Create a new context...
	wf_context:page_context(Page),
	wf_context:handlers(Handlers),
	wf_context:dom_paths(DomPaths),
	
	% Return the new context...
	ok.
	
	
	
%%% SET UP AND TEAR DOWN HANDLERS %%%
	
% init_handlers/1 - 
% Handlers are initialized in the order that they exist in #context.handlers. The order
% is important, as some handlers may depend on others being initialize. For example, 
% the session handler may use the cookie handler to get or set the session cookie.
call_init_on_handlers() ->
	Handlers = wf_context:handlers(),
	F = fun(#handler_context { name=Name, module=Module, state=State }) -> 
		{ok, NewState} = Module:init(State),
		wf_handler:set_handler(Name, Module, NewState)
	end,	
	[F(X) || X <- Handlers].

% finish_handlers/1 - 
% Handlers are finished in the order that they exist in #context.handlers. The order
% is important, as some handlers should finish after others. At the very least,
% the 'render' handler should go last to make sure that it captures all changes
% put in place by the other handlers.
call_finish_on_handlers() ->
	Handlers = wf_context:handlers(),
	F = fun(#handler_context { name=Name, module=Module, state=State }) -> 
		{ok, NewState} = Module:finish(State),
		wf_handler:set_handler(Name, Module, NewState)
	end,
	[F(X) || X <- Handlers].
	
	
	
%%% FIRST REQUEST %%%

run_first_request() ->
	% Some values...
	Module = wf_context:event_module(),
	{module, Module} = code:ensure_loaded(Module),
	Data = Module:main(),
	wf_context:data(Data).


%%% POSTBACK REQUEST %%%

run_postback_request() ->
	% Some values...
	Module = wf_context:event_module(),
	Tag = wf_context:event_tag(),
	
	% Validate...
	{ok, IsValid} = wf_validation:validate(),
	
	% Call the event...
	case IsValid of
		true -> Module:event(Tag);
		false -> ok
	end.
	
%%% BUILD THE RESPONSE %%%

build_static_file_response(Path) ->
	Response = wf_context:response_bridge(),
	Response1 = Response:file(Path),
	Response1:build_response().

build_first_response(Html, Script) ->
	% Update the output with any script...
	Html1 = replace(script, Script, Html),

	% Update the response bridge and return.
	Response = wf_context:response_bridge(),
	Response1 = Response:data(Html1),
	Response1:build_response().
	
build_postback_response(Script) ->
	% Update the response bridge and return.
	Response = wf_context:response_bridge(),
	% TODO - does this need to be flattened?
	Response1 = Response:data(lists:flatten(Script)),
	Response1:build_response().
		
replace(_, _, S) when ?IS_STRING(S) -> S;
replace(Old, New, [Old|T]) -> [New|T];
replace(Old, New, [H|T]) -> [replace(Old, New, H)|replace(Old, New, T)];
replace(_, _, Other) -> Other.
