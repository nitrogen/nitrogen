-module (demos_api).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Javascript API".

headline() -> "Javascript API".

left() -> 
    [
        "
        <p>
        The <code>#api{}</code> element allows you to create a
        Javascript API function on your page that will fire requests
        back to Nitrogen. API postbacks are handled by api_event(Name,
        Tag, Arguments).

        <p>
        The <code>name</code> property specifies the name under which the
        function will be exposed.

        <p>
        The <code>tag</code> property allows you to include an opaque
        value that is passed into the event. It is not exposed to the
        client.

        <p>
        The arguments are an Erlang term that maps to the arguments
        you specified in Javascript. These are true Erlang arguments,
        and can be used for pattern matching. Nitrogen uses <a
        href='http://github.com/rklophaus/BERT-JS'>BERT-JS</a> to
        encode Javascript terms into an Erlang binary.

        <p>
        This demo sets up three Javascript APIs on the page, called
        via the links on the right.
        ",
        linecount:render()
    ].

right() -> 
    wf:wire(#api { name=apiOne, tag=f1 }),
    wf:wire(#api { name=apiTwo, tag=f2 }),
    wf:wire(#api { name=apiThree, tag=f3 }),
    [
        #flash{},
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
