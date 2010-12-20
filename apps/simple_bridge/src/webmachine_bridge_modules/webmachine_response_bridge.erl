%% Simple Bridge
%% Copyright (c) 2008-2010 Rusty Klophaus
%% See MIT-LICENSE for licensing information.

-module (webmachine_response_bridge).
-behaviour (simple_bridge_response).
-include_lib ("simple_bridge.hrl").
-export ([build_response/2]).

build_response(Req, Res) ->	
    Code = Res#response.statuscode,
    case Res#response.data of
        {data, Body} ->
            Size = integer_to_list(httpd_util:flatlength(Body)),

            %% Assemble headers...
            Headers = lists:flatten([
                {content_length, Size},
                [{X#header.name, X#header.value} || X <- Res#response.headers],
                [create_cookie_header(X) || X <- Res#response.cookies]
            ]),		

            Req1 = wrq:set_response_code(Code, Req),
            Req2 = wrq:set_resp_headers(Headers, Req1),
            {ok, Body, Req2};
        {file, Path} ->
            %% We should really never get here, because Webmachine
            %% works via a dispatch table mapping routes to
            %% modules. For this case to happen, it means the dispatch
            %% table mapped a route to a static file.
            throw({not_yet_implemented, {file, Path}})
    end.

create_cookie_header(Cookie) ->
    SecondsToLive = Cookie#cookie.minutes_to_live * 60,
    Expire = to_cookie_expire(SecondsToLive),
    Name = Cookie#cookie.name,
    Value = Cookie#cookie.value,
    Path = Cookie#cookie.path,
    {"Set-Cookie", io_lib:format("~s=~s; Path=~s; Expires=~s", [Name, Value, Path, Expire])}.

to_cookie_expire(SecondsToLive) ->
    Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + SecondsToLive),
    httpd_util:rfc1123_date(DateTime).
