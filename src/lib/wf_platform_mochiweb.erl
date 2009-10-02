% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_platform_mochiweb).
-include ("wf.inc").
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

get_platform() -> mochiweb.

%%% PATH, METHOD, AND ARGS %%%

get_raw_path() ->
	Req = wf_platform:get_request(),
	Req:get(raw_path).

get_querystring() ->
	Req = wf_platform:get_request(),
	RawPath = Req:get(raw_path),
	{_, QueryString, _} = mochiweb_util:urlsplit_path(RawPath),
	QueryString.
	
get_request_method() ->
	Req = wf_platform:get_request(),
	Req:get(method).
	
get_request_body() ->
	undefined.

parse_get_args() ->
	Req = wf_platform:get_request(),
	Req:parse_qs().
	
parse_post_args() ->
	Req = wf_platform:get_request(),
	Req:parse_post().
	
	
	
%%% COOKIES %%%
	
get_cookie(Key) ->
	Req = wf_platform:get_request(), 
	Req:get_cookie_value(Key).

create_cookie(Key, Value, Path, MinutesToLive) ->
	SecondsToLive = MinutesToLive * 60,
	mochiweb_cookies:cookie(Key, Value, [{path, Path}, {max_age, SecondsToLive}]).



%%% HEADERS %%%
create_header(Key, Value) ->
	{Key, Value}.

get_headers() ->
	Req = wf_platform:get_request(),
	F = fun(Header) -> Req:get_header_value(Header) end,
	[
		{connection, F("connection")},
		{accept, F("accept")},
		{host, F("host")},
		{if_modified_since, F("if-modified-since")},
		{if_match, F("if-match")},
    {if_none_match, F("if-range")},
    {if_unmodified_since, F("if-unmodified-since")},
    {range, F("range")},
		{referer, F("referer")},
    {user_agent, F("user-agent")},
    {accept_ranges, F("accept-ranges")},
    {cookie, F("cookie")},
    {keep_alive, F("keep-alive")},
    {location, F("location")},
    {content_length, F("content-length")},
    {content_type, F("content-type")},
    {content_encoding, F("content-encoding")},
    {authorization, F("authorization")},
    {x_forwarded_for, F("x-forwarded-for")},
    {transfer_encoding, F("transfer-encoding")}
	].

get_header(Header) ->
	Headers = get_headers(),
	proplists:get_value(Header, Headers).	
	
%%% RESPONSE %%%

build_response() ->
	% Get vars...
	Req = wf_platform:get_request(),
	Code = get(wf_response_code),
	ContentType = get(wf_content_type),
	Body = get(wf_response_body),

	% Set the content-type...
	wf_platform:set_header("Content-Type", ContentType),
	Headers = get(wf_headers),
	
	% Send the mochiweb response...
	Req:respond({Code, Headers, Body}).

%%% SOCKETS %%%

get_socket() ->
	Req = wf_platform:get_request(),
	Req:get(socket).

recv_from_socket(Length, Timeout) -> 
	Socket = get_socket(),
	case gen_tcp:recv(Socket, Length, Timeout) of
		{ok, Data} -> 
			put(mochiweb_request_recv, true),
			Data;
		_Other -> 
			exit(normal)
	end.
