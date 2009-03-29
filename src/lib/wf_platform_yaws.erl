% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_platform_yaws).
-include ("yaws_api.hrl").
-export ([
	get_platform/0,
	
	get_raw_path/0,
	get_querystring/0,
	get_request_method/0,
	get_request_body/0,
	
	parse_get_args/0,
	parse_post_args/0,
	
	get_cookie/1,
	create_cookie/4,
	
	create_header/2,
	get_headers/0,
	get_header/1,
	
	build_response/0,

	get_socket/0,
	recv_from_socket/2
]).

get_platform() -> yaws.

%%% PATH, METHOD, AND ARGS %%%

get_raw_path() ->
	Arg = wf_platform:get_request(),
	wf:f("~s?~s", [Arg#arg.server_path, wf:to_list(Arg#arg.querydata)]).
	
get_querystring() ->
	Arg = wf_platform:get_request(),
	Arg#arg.querydata.

get_request_method() ->
	Arg = wf_platform:get_request(),
	(Arg#arg.req)#http_request.method.
	
get_request_body() ->
	Arg = wf_platform:get_request(),
	case Arg#arg.clidata of
		{partial, Data} -> Data;
		Data -> Data
	end.
	
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
	
get_headers() ->
	Arg = wf_platform:get_request(),
	Headers = Arg#arg.headers,
	[
		{connection, Headers#headers.connection},
		{accept, Headers#headers.accept},
		{host, Headers#headers.host},
		{if_modified_since, Headers#headers.if_modified_since},
		{if_match, Headers#headers.if_match},
    {if_none_match, Headers#headers.if_range},
    {if_unmodified_since, Headers#headers.if_unmodified_since},
    {range, Headers#headers.range},
		{referer, Headers#headers.referer},
    {user_agent, Headers#headers.user_agent},
    {accept_ranges, Headers#headers.accept_ranges},
    {cookie, Headers#headers.cookie},
    {keep_alive, Headers#headers.keep_alive},
    {location, Headers#headers.location},
    {content_length, Headers#headers.content_length},
    {content_type, Headers#headers.content_type},
    {content_encoding, Headers#headers.content_encoding},
    {authorization, Headers#headers.authorization},
    {transfer_encoding, Headers#headers.transfer_encoding}
	].

get_header(Header) ->
	Headers = get_headers(),
	proplists:get_value(Header, Headers).
	
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


%%% SOCKETS %%%

get_socket() ->
	Arg = wf_platform:get_request(),
	Arg#arg.clisock.

recv_from_socket(Length, Timeout) -> 
	Socket = get_socket(),
	case gen_tcp:recv(Socket, Length, Timeout) of
		{ok, Data} -> Data;
		_ -> exit(normal)
	end.
