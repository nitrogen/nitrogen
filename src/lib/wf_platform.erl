% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_platform).
-include ("wf.inc").
-export ([
	init/2,
	get_platform/0,
	get_request/0,
	get_page_module/0, set_page_module/1,

	get_raw_path/0,
	get_querystring/0,
	request_method/0,
	parse_get_args/0,
	parse_post_args/0,
	
	get_cookie/1, set_cookie/2, set_cookie/4,
	clear_redirect/0,
	set_redirect/1,
	set_header/2,
	set_response_code/1,
	set_content_type/1,
	set_response_body/1,
	build_response/0,
	
	inject_script/2
]).


%%% INIT PLATFORMS %%%
init(Platform, Request) ->
	put(wf_platform, Platform),
	put(wf_request, Request),
	ok.
	
do(Method) ->
	case get_platform() of
		yaws     -> wf_platform_yaws:Method();
		mochiweb -> wf_platform_mochiweb:Method();
		inets    -> wf_platform_inets:Method()
	end.

do(Method, Args) ->
	case get_platform() of
		yaws     -> erlang:apply(wf_platform_yaws, Method, Args);
		mochiweb -> erlang:apply(wf_platform_mochiweb, Method, Args);
		inets    -> erlang:apply(wf_platform_inets, Method, Args)
	end.



%%% GET PLATFORM INFO %%%

get_platform() -> get(wf_platform).
get_request() -> get(wf_request).
get_page_module() -> get(wf_page_module).
set_page_module(Module) -> put(wf_page_module, Module).

get_raw_path() -> do(get_raw_path).
get_querystring() -> do(get_querystring).

%%% METHOD AND ARGS %%%

request_method() -> do(request_method).
parse_get_args() -> do(parse_get_args).
parse_post_args() -> do(parse_post_args).
	
%%% COOKIES %%%
	
get_cookie(Key) ->
	Key1 = wf:to_list(Key),
	Value = do(get_cookie, [Key1]),
	case Value of
		[] -> undefined;
		_ -> Value
	end.

set_cookie(Key, Value) -> 
	Timeout = wf_global:session_timeout(),
	set_cookie(Key, Value, "/", Timeout).
	
set_cookie(Key, Value, Path, MinutesToLive) ->
	Header = create_cookie(Key, Value, Path, MinutesToLive),
	put(wf_headers, [Header|get(wf_headers)]),
	ok.
	
create_cookie(Key, Value, Path, MinutesToLive) ->
	Key1 = wf:to_list(Key),
	Value1 = wf:to_list(Value),
	do(create_cookie, [Key1, Value1, Path, MinutesToLive]).
	


%%% HEADERS %%%

set_header(Key, Value) ->
	Key1 = wf:to_list(Key),
	Value1 = wf:to_list(Value),
	Header = do(create_header, [Key1, Value1]),
	put(wf_headers, [Header|get(wf_headers)]),
	ok.
	
	
	
%%% RESPONSE %%%

clear_redirect() -> set_redirect(undefined).
set_redirect(Url) -> put(wf_redirect, Url).
set_response_code(Code) -> put(wf_response_code, Code).
set_content_type(ContentType) -> put(wf_content_type, ContentType).

set_response_body(Body) -> put(wf_response_body, Body).
	
build_response() -> 
	% Handle any redirects...
	handle_redirects(),
	
	% Build platform specific response...
	do(build_response).
	
%%% REDIRECTS %%%
	
handle_redirects() ->
	case get(wf_redirect) of
		undefined -> ignore;
		Url -> 
			Redirect = build_redirect(Url),
			set_response_body(Redirect)
	end.

build_redirect(Url) ->
	Url1 = wf:to_list(Url),
	case request_method() of
		'GET' -> build_get_redirect(Url1);
		'POST' -> build_post_redirect(Url1)
	end.

build_get_redirect(Url) -> wf:f("<meta http-equiv='refresh' content='0;url=~s'>", [Url]).
build_post_redirect(Url) -> wf:f("document.location.href=\"~s\";", [wf_utils:js_escape(Url)]).

%%% INJECT SCRIPT %%%

inject_script(undefined, _) -> undefined;
inject_script([script|Rest], Script) -> [Script, Rest];
inject_script([Other|Rest], Script) -> [Other|inject_script(Rest, Script)];
inject_script([], _Script) -> [].


%%% CHUNKED RESPONSE %%%

