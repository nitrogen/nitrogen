<h1>SimpleBridge</h1>

SimpleBridge takes the pain out of coding to multiple Erlang HTTP servers by creating a standardized interface. It currently supports Mochiweb, Inets, and Misultin, with Yaws coming soon.

SimpleBridge is used in the experimental branch of the <a href="http://nitrogenproject.com">The Nitrogen Web Framework</a>.

In a sense, it is similar to <a href="http://github.com/skarab/ewgi">EWGI</a>, except SimpleBridge has some key improvements/differences:

* <b>Smaller code base</b> - SimpleBridge is 870 lines, compared to 1,974 lines for EWGI. This is because SimpleBridge uses the underlying HTTP server's built in parsing functions as much as possible.
* <b>Easily extended</b> - Takes about 150 lines to add support for a new HTTP server, vs. ~350 for EWGI.
* <b>MultiPart File Uploads</b> - SimpleBridge has better support for HTTP POSTS, including support for multipart file uploads, with size limits and handle-able errors.
* <b>Static File Support</b> - Support for sending a static file to the browser, using the underlying HTTP server's own methods.
* <b>Cookies Support</b> - SimpleBridge provides interface functions for getting and setting cookies.
* <b>No Middleware Components</b> - SimpleBridge does not explicitly support EWGI's concept of "middleware components". (Though you could probably fake it, haven't tried.)

SimpleBridge is split into two parts: 

* A *Request Bridge* is a parameterized module that allows you to see information about the incoming request.
* A *Response Bridge* is a parameterized module that allows you to construct a response.


</h2>Hello World Example</h2>


    % SimpleBridge Hello World Example in Mochiweb

    start(_, _) ->
    	Options = [{ip, "127.0.0.1"}, {port, 8000}],
    	Loop = fun loop/1,
    	mochiweb_http:start([{name, mochiweb_example_app}, {loop, Loop} | Options]).

    loop(Req) ->
    	Request = simple_bridge:make_request(mochiweb_request_bridge, {Req, "./wwwroot"}),
    	HTML = [
    		"&lt;h1&gt;Hello, World!&lt;/h1&gt;",
    		io_lib:format("METHOD: ~p~n&lt;br&gt;&lt;br&gt;", [Request:request_method()]),
    		io_lib:format("COOKIES: ~p~n&lt;br&gt;&lt;br&gt;", [Request:cookies()]),
    		io_lib:format("HEADERS: ~p~n&lt;br&gt;&lt;br&gt;", [Request:headers()]),
    		io_lib:format("QUERY PARAMETERS: ~p~n&lt;br&gt;&lt;br&gt;", [Request:query_params()])		
    	],

    	Response = simple_bridge:make_response(mochiweb_response_bridge, {Req, "./wwwroot"}),		
    	Response1 = Response:status_code(200),
    	Response2 = Response1:header("Content-Type", "text/html"),
    	Response3 = Response2:data(HTML),
    	Response3:build_response().


<h2>Request Bridges</h2>
<h3>How do I make a request bridge?</h3>

To make a request bridge for an incoming request, call the simple_bridge:make_request/2 function,
specifying the appropriate bridge module for your HTTP server, and the arguments that it needs. This code would sit in the loop/1 function of a Mochiweb server, or the do/1 function of an Inets server.

Inets example:

	RequestBridge = simple_bridge:make_request(inets_response_bridge, Info)

Mochiweb example:

	RequestBridge = simple_bridge:make_request(mochiweb_response_bridge, [{Req, Docroot}]).
	
	
<h3>What can I do with the request bridge object?</h3>

Once you have created the request bridge object (a parameterized module), it provides you with a standard interface for accessing the request method, path, query parameters, post parameters, headers, and cookies of the request:


<h3>Request Bridge Interface</h3>

* *Bridge:request_method()* - returns 'GET', 'POST', 'HEAD', etc.
* *Bridge:path()* - returns the requested path and file.
* *Bridge:peer_ip()* - returns the client's IP address in tuple format (74.125.67.100 = {74, 125, 67, 100})
* *Bridge:peer_port()* - returns the client's port.
* *Bridge:headers()* - returns a proplist of headers, [{header1, "Value1"}, {header2, "Value2"}, ...]
* *Bridge:header(Header)* - returns the value of a header.
* *Bridge:cookies()* - returns a proplist of cookies, [{"Cookie1", "Value1"}, {"Cookie2", "Value2"}, ...]
* *Bridge:query_params()* - returns a proplist of query params, [{"Query1", "Value1"}, {"Query2", "Value2"}, ...]
* *Bridge:post_params()* - returns a proplist of post params, [{"Post1", "Value1"}, {"Post2", "Value2"}, ...]
* *Bridge:post_files()* - returns a list of upload_file records, describing the files uploaded in a multipart post. 
* *Bridge:request_body()* - returns the request body that has been read so far, as a list.
* *Bridge:error()* - returns an Erlang term describing any errors that happened while parsing a multipart post.


<h3>What modules are involved in a request bridge?</h3>

* *request_bridge.erl* - The behaviour interface that request bridge modules must implement.
* *request_bridge_wrapper.erl* - A parameterized module that wraps a request. 
* *inets_request_bridge.erl* - The request bridge module for Inets.
* *mochiweb_request_bridge.erl* - The request bridge module for Mochiweb.
* *???_request_bridge.erl* - Support for more servers on the way.

To extend the SimpleBridge to work with other HTTP servers (or other versions of Inets, Mochiweb, or Yaws), copy and modify inets_request_bridge.erl or mochiweb_request_bridge.erl.


<h2>Response Bridges</h2>
<h3>How do I make a response bridge?</h3>

To make a request bridge for an incoming request, call the simple_bridge:make_response/2 function,
specifying the appropriate bridge module for your HTTP server, and the arguments that it needs. This code would sit in the loop/1 function of a Mochiweb server, or the do/1 function of an Inets server.

Inets example:

    ResponseBridge = simple_bridge:make_response(inets_response_bridge, Info)

Mochiweb example:

    ResponseBridge = simple_bridge:make_response(mochiweb_response_bridge, {Req, Docroot})


<h3>What can I do with the Response Bridge?</h3>

Once you have created the request bridge object (a parameterized module), it provides you with a standard interface for combining headers, cookies, and a response body into a response appropriate for your http server. 

Each function below returns a new bridge object, so your will need to 
chain together requests like this:

	Bridge = simple_bridge:make_response(inets_response_bridge, Info),
	Bridge1 = Bridge:status_code(200),
	Bridge2 = Bridge1:header("Header1", "Value1"),
	Bridge3 = Bridge2:data(HTML),
	etc.
	
	
<h3>Response Bridge Interface</h3>

* *Bridge:status_code(Code)* - set the HTTP status code. (200, 404, etc.)
* *Bridge:header(Name, Value)* - set an HTTP header.
* *Bridge:clear_headers()* - clear all previously set headers.
* *Bridge:cookie(Name, Value)* - set a cookie for path "/" with expiration in 20 minutes.
* *Bridge:cookie(Name, Value, Path, Exp)* - Set a cookie. Exp is an integer in minutes.
* *Bridge:clear_cookies()* - clear all previously set cookies.
* *Bridge:data(Data)* - set the data to return in the response. Usually HTML goes here.
* *Bridge:file(File)* - Send a static file to the browser.

Finally, you build the response to send to your HTTP server with the build_response/0 function.

* *Bridge:build_response()* - Create a response tuple that you can hand off to your HTTP server.


<h3>What modules are involved in a response bridge?</h3>

* *response_bridge.erl* - The behaviour interface that response bridge modules must implement.
* *response_bridge_wrapper.erl* - A parameterized module that wraps a response. 
* *inets_response_bridge.erl* - The response bridge module for Inets.
* *mochiweb_response_bridge.erl* - The response bridge module for Mochiweb.
* *???_response_bridge.erl* - Support for more servers on the way.

To extend the SimpleBridge to other HTTP servers (or other versions of Inets, Mochiweb, or Yaws), 
copy and modify inets_response_bridge.erl or mochiweb_response_bridge.erl.
