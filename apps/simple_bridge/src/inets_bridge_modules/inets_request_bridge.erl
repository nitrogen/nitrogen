% Simple Bridge
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (inets_request_bridge).
-behaviour (simple_bridge_request).
-include ("httpd_r12b5.hrl").
-include ("simplebridge.hrl").
-export ([
	init/1,
	request_method/1, path/1,
	peer_ip/1, peer_port/1,
	headers/1, cookies/1,
	query_params/1, post_params/1, request_body/1,
	socket/1, recv_from_socket/3
]).


init(Req) -> 
	Req.

request_method(Req) -> 
	list_to_atom(Req#mod.method).

path(Req) -> 
	{Path, _QueryString} = split_request_uri(Req#mod.request_uri, []),
	Path.

peer_ip(Req) -> 
	Socket = Req#mod.socket,
	{ok, {IP, _Port}} = inet:peername(Socket),
	IP.
	
peer_port(Req) -> 
	Socket = Req#mod.socket,
	{ok, {_IP, Port}} = inet:peername(Socket),
	Port.
	
headers(Req) ->
	Headers = Req#mod.parsed_header,
	F = fun(Header) -> proplists:get_value(Header, Headers) end,
	Headers1 = [
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
    {transfer_encoding, F("transfer-encoding")}
	],
	[{K, V} || {K, V} <- Headers1, V /= undefined].
	
cookies(Req) ->
	Headers = headers(Req),
	CookieData = proplists:get_value(cookie, Headers, ""),
	F = fun(Cookie) ->
		case string:tokens(Cookie, "=") of
			[] -> [];
			L -> 
				X = string:strip(hd(L)),
				Y = string:join(tl(L), "="),
				{X, Y}
		end
	end,
	[F(X) || X <- string:tokens(CookieData, ";")].
	
query_params(Req) ->
	{_Path, QueryString} = split_request_uri(Req#mod.request_uri, []),
	Query = httpd:parse_query(QueryString),
	[{Key, Value} || {Key, Value} <- Query, Key /= []].
	
post_params(Req) ->
	Body = request_body(Req),
	Query = httpd:parse_query(Body),
	[{Key, Value} || {Key, Value} <- Query, Key /= []].

request_body(Req) ->
	Req#mod.entity_body.

socket(Req) -> 
	Req#mod.socket.

recv_from_socket(Length, Timeout, Req) -> 
	Socket = socket(Req),
	case gen_tcp:recv(Socket, Length, Timeout) of
		{ok, Data} -> Data;
		_ -> exit(normal)
	end.


%%% PRIVATE FUNCTIONS %%%
split_request_uri([], Path) -> {lists:reverse(Path), ""};
split_request_uri([$?|QueryString], Path) -> {lists:reverse(Path), QueryString};
split_request_uri([H|T], Path) -> split_request_uri(T,[H|Path]).
	