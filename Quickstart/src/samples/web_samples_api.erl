-module (web_samples_api).
-include ("wf.inc").
-include ("google_chart.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Javascript API Example".
headline() -> "Javascript API Example".
right() -> linecount:render().

body() -> 
	?PRINT(wf:q("test")),
	wf:wire(#api { name=apiOne, tag=f1 }),
	wf:wire(#api { name=apiTwo, tag=f2 }),
	wf:wire(#api { name=apiThree, tag=f3 }),
	[
	"
		The #api { name=FunctionName, tag=Term } element allows you to create a Javascript API function on 
		your page that will fire requests back to Nitrogen. API postbacks are handled by 
		api_event(Name, Tag, Arguments). 
		<p>
		The 'name' property of #api specifies the name under which the function will be exposed.
		<p>
		The 'tag' property of #api allows you to include an opaque value that is passed into the event. It is not
		exposed to the client.
		<p>
		The arguments are an Erlang term that maps to the arguments you specified in Javascript. These are true 
		Erlang arguments, and can be used for pattern matching. View the source of this page to see the
		magic.
	",
	#p{},
	"<a onclick=\"page.apiOne('Hello Joe!');\">page.apiOne('Hello Joe!')</a><br>"
	"<a onclick=\"page.apiTwo({ greeting:'Hello', name:'Mike' });\">page.apiTwo({ greeting:'Hello', name:'Mike' })</a><br>"
	"<a onclick=\"page.apiThree(Bert.atom('hello'), Bert.atom('robert'), 12345);\">page.apiThree(Bert.atom('hello'), Bert.atom('robert'), 12345)</a>"
	].

% Notice the argument pattern matching!	
api_event(apiOne, _, ["Hello Joe!"]) ->
	wf:flash("Hello Joe!");
	
api_event(apiTwo, _, [[{greeting, "Hello"}, {name, "Mike"}]]) ->
	wf:flash("Hello Mike!");
	
api_event(apiThree, _, [hello, robert, 12345]) ->
	wf:flash("Hello Robert!");
	
api_event(A, B, C) ->
	?PRINT(A), ?PRINT(B), ?PRINT(C).

	
event(_) -> ok.