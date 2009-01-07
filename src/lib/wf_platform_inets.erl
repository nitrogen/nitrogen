% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_platform_inets).
-include ("httpd.hrl").
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

get_platform() -> inets.

%%% PATH, METHOD, AND ARGS %%%

get_raw_path() ->
	Info = wf_platform:get_request(),
	Info#mod.request_uri.

get_querystring() ->
	Info = wf_platform:get_request(),
	{_Path, QueryString} = httpd_util:split_path(Info#mod.request_uri),
	case QueryString of 
		[] -> []; 
		_ -> tl(QueryString) 
	end.

request_method() ->
	Info = wf_platform:get_request(),
	wf:to_atom(Info#mod.method).

parse_get_args() ->
	QueryString = get_querystring(),
	Query = httpd:parse_query(QueryString),
	[{Key, Value} || {Key, Value} <- Query, Key /= []].

parse_post_args() ->
	Info = wf_platform:get_request(),
	Query = httpd:parse_query(Info#mod.entity_body),
	[{Key, Value} || {Key, Value} <- Query, Key /= []].


	
%%% COOKIES %%%
	
get_cookie(Key) ->
	Key1 = wf:to_list(Key),
	Info = wf_platform:get_request(),
	Headers = Info#mod.parsed_header,
	CookieData = proplists:get_value("cookie", Headers, ""),
	F = fun(Cookie) ->
		case string:tokens(Cookie, "=") of
			[] -> [];
			L -> 
				X = string:strip(hd(L)),
				Y = string:join(tl(L), "="),
				{X, Y}
		end
	end,
	Cookies = [F(X) || X <- string:tokens(CookieData, ";")],
	proplists:get_value(Key1, Cookies).
	
create_cookie(Key, Value, Path, MinutesToLive) ->
	SecondsToLive = MinutesToLive * 60,
	Expire = to_cookie_expire(SecondsToLive),
	{"Set-Cookie", wf:f("~s=~s; Path=~s; Expires=~s", [Key, Value, Path, Expire])}.
	
to_cookie_expire(SecondsToLive) ->
	Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	DateTime = calendar:gregorian_seconds_to_datetime(Seconds + SecondsToLive),
	httpd_util:rfc1123_date(DateTime).



%%% HEADERS %%%
create_header(Key, Value) ->
	{Key, Value}.
	

%%% RESPONSE %%%
	
build_response() ->
	% Get vars...
	Info = wf_platform:get_request(),
	ResponseCode = get(wf_response_code),
	ContentType = get(wf_content_type),
	Body = get(wf_response_body),
	Size = integer_to_list(httpd_util:flatlength(Body)),
	
	% Assemble headers...
	Headers = lists:flatten([
		{code, ResponseCode},
		{content_type, ContentType},
		{content_length, Size},
		get(wf_headers)
	]),		

	% Send the inets response...
	{proceed,[
		{response, {response, Headers, Body}},
		{mime_type, ContentType} | Info#mod.data
	]}.

