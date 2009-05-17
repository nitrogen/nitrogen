-module (web_req).
-include ("wf.inc").
-compile(export_all).

main() ->	#template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

body() ->
	wf:set_cookie("cookie1", "value1"),
	wf:set_cookie("cookie2", "value2"),
	wf:set_cookie("cookie3", "value3"),
	[
		#panel { id=pre, body=[
			"<pre>",
			wf:f("~p", [wf_platform:get_request()]),
			"</pre>"
		]},
		#link { text="Link", url="/web/req?query1=value1&query2=value2&query3=value3" },
		#textbox { id="post1", text="value1" },
		#textbox { id="post2", text="value2" },
		#textbox { id="post3", text="value3" },
		#link { text="Post", postback=post }
	].
	
event(post) ->
	wf:update(pre, 
	[
		"<pre>",
		wf:f("~p", [wf_platform:get_request()]),
		"</pre>"
	]).
	