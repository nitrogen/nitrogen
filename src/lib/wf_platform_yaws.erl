% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_platform_yaws).
-include ("yaws_api.hrl").
-export ([
	get_platform/0,
	
	get_raw_path/0,
	get_querystring/0,
	request_method/0,
	
	parse_get_args/0,
	parse_post_args/0,
	
	get_cookie/1,
	create_cookie/4,
	
	create_header/2,
	
	build_response/0
]).

get_platform() -> yaws.

%%% PATH, METHOD, AND ARGS %%%

get_raw_path() ->
	Arg = wf_platform:get_request(),
	wf:f("~s?~s", [Arg#arg.server_path, wf:to_list(Arg#arg.querydata)]).
	
get_querystring() ->
	Arg = wf_platform:get_request(),
	Arg#arg.querydata.

request_method() ->
	Arg = wf_platform:get_request(),
	(Arg#arg.req)#http_request.method.
	
parse_get_args() ->
	Arg = wf_platform:get_request(),
	yaws_api:parse_query(Arg).

parse_post_args() ->
	Arg = wf_platform:get_request(),
	yaws_api:parse_post(Arg).
		
		
		
%%% COOKIES %%%
	
get_cookie(Key) ->
	Arg = wf_platform:get_request(),
	Key1 = wf:to_list(Key),
	Headers = Arg#arg.headers,
	yaws_api:find_cookie_val(Key1, Headers#headers.cookie).
	
create_cookie(Key, Value, Path, MinutesToLive) ->
	SecondsToLive = MinutesToLive * 60,
	Expire = to_cookie_expire(SecondsToLive),
	yaws_api:setcookie(Key, Value, Path, Expire).
	
to_cookie_expire(SecondsToLive) ->
	Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	DateTime = calendar:gregorian_seconds_to_datetime(Seconds + SecondsToLive),
	httpd_util:rfc1123_date(DateTime).



%%% HEADERS %%%

create_header(Key, Value) ->
	{header, {Key, Value}}.
	
	
	
%%% RESPONSE %%%

build_response() ->
	% Get vars...
	ContentType = get(wf_content_type),
	Body = get(wf_response_body),

	% Send the yaws response...
	lists:flatten([
		{status, get(wf_response_code)},
		get(wf_headers),
		{content, ContentType, Body}
	]).