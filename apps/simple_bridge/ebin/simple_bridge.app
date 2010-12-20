{application, simple_bridge, [
    {description,  "SimpleBridge - Common Interface to Erlang HTTP Servers"},
    {vsn,          "1.1.0"},
    {modules,      [
        inets_request_bridge,
        inets_response_bridge,
        misultin_request_bridge,
        misultin_response_bridge,
        mochiweb_request_bridge,
        mochiweb_response_bridge,
        simple_bridge,
        simple_bridge_multipart,
        simple_bridge_request,
        simple_bridge_request_wrapper,
        simple_bridge_response,
        simple_bridge_response_wrapper,
        yaws_request_bridge,
        yaws_response_bridge
    ]},
    {applications, [kernel, stdlib, sasl]},
    {registered, []}
]}.
