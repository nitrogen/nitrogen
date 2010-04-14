-module (inets_request_bridge_tests).
-include ("test.hrl").
-export ([
    request_method_test/0,
    url_test/0,
    peer_test/0,
    headers_test/0,
    cookies_test/0,
    query_params_test/0,
    post_params_test/0
]).

request_method_test() ->
    Bridge = utils:make_inets_get_bridge(),
    'GET' = Bridge:request_method().

url_test() ->
    Bridge = utils:make_inets_get_bridge(),
    "/web/req" = Bridge:path().

peer_test() ->
    Bridge = utils:make_inets_get_bridge(),
    ?PEER_IP = Bridge:peer_ip(),
    ?PEER_PORT = Bridge:peer_port().

headers_test() ->
    Bridge = utils:make_inets_get_bridge(),
    Headers = Bridge:headers(),
    utils:verify(connection, "keep-alive", Headers),
    utils:verify(keep_alive, "300", Headers).

cookies_test() ->
    Bridge = utils:make_inets_get_bridge(),
    Cookies = Bridge:cookies(),
    utils:verify("cookie1", "value1", Cookies),
    utils:verify("cookie2", "value2", Cookies),
    utils:verify("cookie3", "value3", Cookies).

query_params_test() ->
    Bridge = utils:make_inets_get_bridge(),
    Params = Bridge:query_params(),
    utils:verify("query1", "value1", Params),
    utils:verify("query2", "value2", Params),
    utils:verify("query3", "value3", Params).

post_params_test() ->
    Bridge = utils:make_inets_post_bridge(),
    Params = Bridge:post_params(),
    utils:verify("post1", "value1", Params),
    utils:verify("post2", "value2", Params),
    utils:verify("post3", "value3", Params).
