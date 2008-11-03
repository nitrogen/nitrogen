-module (wf_platform).
-include ("wf.inc").
-include ("yaws_api.hrl").
-export ([
	init_mochiweb_request/1,
	init_yaws_request/1,
	get_platform/0,
	get_platform_and_request/0,
	request_method/0,
	parse_get_args/0,
	parse_post_args/0,
	
	get_cookie/1, set_cookie/2, set_cookie/4,
	set_redirect/1,
	set_header/2,
	set_response_code/1,
	set_content_type/1,
	set_response_body/1,
	build_response/0
]).


%%% INIT PLATFORMS %%%
init_mochiweb_request(Req) ->
	erlang:erase(),
	put(wf_platform, mochiweb),
	put(mochiweb_request, Req),
	ok.
	
init_yaws_request(Arg) ->
	put(wf_platform, yaws),
	put(yaws_request, Arg),
	ok.

%%% GET PLATFORM INFO %%%

get_platform() -> get(wf_platform).

get_platform_and_request() -> 
	case get_platform() of
		mochiweb -> { mochiweb, get(mochiweb_request) };
		yaws ->     { yaws, get(yaws_request) }
	end.


%%% METHOD AND ARGS %%%

request_method() ->
	case get_platform_and_request() of
		{mochiweb,Req} -> Req:get(method);
		{yaws, Arg} ->    Arg#http_request.method
	end.

parse_get_args() ->
	case get_platform_and_request() of
		{mochiweb, Req} -> Req:parse_qs();
		{yaws, Arg} ->     yaws_api:parse_query(Arg)
	end.

parse_post_args() ->
	case get_platform_and_request() of
		{mochiweb, Req} -> Req:parse_post();
		{yaws, Arg} ->     yaws_api:parse_post(Arg)
	end.
	
%%% COOKIES %%%
	
get_cookie(Key) ->
	Key1 = wf:to_list(Key),
	Value = case get_platform_and_request() of
		{mochiweb, Req} -> 
			Req:get_cookie_value(Key1);
		{yaws, Arg} ->
			Headers = Arg#arg.headers,
			yaws_api:find_cookie_val(Key1, Headers#headers.cookie)
	end,
	case Value of
		[] -> undefined;
		_ -> Value
	end.

set_cookie(Key, Value) -> 
	Header = create_cookie(Key, Value),
	put(wf_headers, [Header|get(wf_headers)]),
	ok.
	
set_cookie(Key, Value, Path, SecondsToLive) ->
	Header = create_cookie(Key, Value, Path, SecondsToLive),
	put(wf_headers, [Header|get(wf_headers)]),
	ok.
	
create_cookie(Key, Value) ->
	Key1 = wf:to_list(Key),
	Value1 = wf:to_list(Value),
	
	case get_platform() of
		mochiweb -> 
			mochiweb_cookies:cookie(Key1, Value1);
		yaws -> 
			yaws_api:setcookie(Key1, Value1)
	end.

create_cookie(Key, Value, Path, SecondsToLive) ->
	Key1 = wf:to_list(Key),
	Value1 = wf:to_list(Value),
	
	case get_platform() of
		mochiweb -> 
			mochiweb_cookies:cookie(Key1, Value1, [{path, Path}, {max_age, SecondsToLive}]);
		yaws -> 
			Expire = to_cookie_expire(SecondsToLive),
			yaws_api:setcookie(Key1, Value1, Path, Expire)
	end.
	
to_cookie_expire(SecondsToLive) ->
	Seconds = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	DateTime = calendar:gregorian_seconds_to_datetime(Seconds + SecondsToLive),
	httpd_util:rfc1123_date(DateTime).

%%% HEADERS %%%
set_header(Key, Value) ->
	Key1 = wf:to_list(Key),
	Value1 = wf:to_list(Value),
	Header = case get_platform() of
		mochiweb -> {Key1, Value1};
		yaws -> {header, {Key1, Value1}}
	end,
	put(wf_headers, [Header|get(wf_headers)]),
	ok.
	
%%% RESPONSE %%%

set_redirect(Url) -> put(wf_redirect, Url).
set_response_code(Code) -> put(wf_response_code, Code).
set_content_type(ContentType) -> put(wf_content_type, ContentType).
	
set_response_body(Body) -> put(wf_response_body, Body).
	
build_response() -> 
	case get_platform() of 
		mochiweb -> build_mochiweb_response();
		yaws -> build_yaws_response()
	end.

build_mochiweb_response() ->
	{mochiweb, Req} = get_platform_and_request(),
	
	% Prepare the body...
	ContentType = get(wf_content_type),
	Body = get(wf_response_body),
	Body1 = case ContentType of
		"text/html" -> 
			Script = wf_script:get_script(),
			inject_script(Body, Script);
		_ -> 
			set_header("Content-Type", ContentType),
			Body
	end,
	
	% Get headers...
	Code = get(wf_response_code),
	Headers = get(wf_headers),
	
	% Respond.
	Req:respond({Code, Headers, Body1}).
	
build_yaws_response() ->
	% Prepare the body...
	ContentType = get(wf_content_type),
	Body = get(wf_response_body),
	Body1 = case ContentType of
		"text/html" ->
			Script = wf_script:get_script(),
			inject_script(Body, Script);
		_ -> 
			Body
	end,
	
	lists:flatten([
		{status, get(wf_response_code)},
		get(wf_headers),
		{content, ContentType, Body1}
	]).

inject_script(undefined, _) -> undefined;
inject_script([script|Rest], Script) -> [Script, Rest];
inject_script([Other|Rest], Script) -> [Other|inject_script(Rest, Script)];
inject_script([], Script) -> 
	?LOG("Missing script tag in template.", []),
	"\r\n<script>\r\n" ++ Script ++ "\r\n</script>\r\n".

