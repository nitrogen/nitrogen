-module (inets_response_bridge_tests).
-include ("test.hrl").
-export ([
    build_response_test/0
]).


build_response_test() ->
    B00 = simple_bridge:make_response(inets_response_bridge, undefined),
    B01 = B00:status_code(200),
    B02 = B01:header("header1", "value1"),
    B03 = B02:header("header2", "value2"),
    B04 = B03:cookie("cookie1", "value1"),
    B05 = B04:cookie("cookie2", "value2"),
    B06 = B05:data("This is the data."),
    _Response = B06:build_response().
