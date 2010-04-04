-module (demos_proxied).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Proxied Example".
headline() -> "Proxied Example".
right() -> linecount:render().

body() -> 
	[
		"
		Nitrogen stores postback state within the browser, allowing you to load-balance
		across different servers at will without worrying about breaking your site's functionality.
		<p>
		Press the 'Postback' button repeatedly to see which node you are on.
		<p>
		Note: For this example to work, you must have multiple versions of Nitrogen 
		running in a proxied environment, and depending on which process_registry_handler
		you use, you will need to ensure that the nodes are all connected.
		",
		#p{},
		#button { text="Postback", postback=postback },
		#p{},
		#panel { id=server }
	].

event(postback) ->
	Count = wf_utils:coalesce([wf:session(count), 0]),
	wf:session(count, Count + 1),
	wf:insert_top(server, [
		#panel { body=wf:f("You are on server '~s'. [~p]<br>", [node(), Count]), actions=#effect { effect=highlight } }
	]).
