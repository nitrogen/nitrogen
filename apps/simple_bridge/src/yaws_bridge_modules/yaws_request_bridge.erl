% Simple Bridge
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(yaws_request_bridge).
-include ("yaws_api.hrl").
-include("simplebridge.hrl").
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

request_method(Arg) ->
  (Arg#arg.req)#http_request.method.

path(Arg) ->
  Arg#arg.server_path.

peer_ip(Arg) -> 
  Socket = socket(Arg),
  {ok, {IP, _Port}} = inet:peername(Socket),
  IP.

peer_port(Arg) -> 
  Socket = socket(Arg),
  {ok, {_IP, Port}} = inet:peername(Socket),
  Port.

headers(Arg) ->
  Headers = Arg#arg.headers,
  [
    {connection, Headers#headers.connection},
    {accept, Headers#headers.accept},
    {host, Headers#headers.host},
    {if_modified_since, Headers#headers.if_modified_since},
    {if_match, Headers#headers.if_match},
    {if_none_match, Headers#headers.if_none_match},
    {if_range, Headers#headers.if_range},
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

cookies(Req) ->
  Headers = Req#arg.headers,
  CookieData = Headers#headers.cookie,
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

query_params(Arg) ->
  yaws_api:parse_query(Arg).

post_params(Arg) ->
  yaws_api:parse_post(Arg).

request_body(Arg) ->
  case Arg#arg.clidata of
    {partial, Data} -> Data;
    Data -> Data
  end.  

socket(Arg) ->
  Arg#arg.clisock.

recv_from_socket(Arg, Length, Timeout) -> 
  Socket = socket(Arg),
  case gen_tcp:recv(Socket, Length, Timeout) of
    {ok, Data} -> Data;
    _ -> exit(normal)
  end.
