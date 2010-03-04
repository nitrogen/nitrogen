-module (web_reference_api).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/onecolumn.html", bindings=[
	{'Group', reference},
	{'Item', api}
]}.

title() -> "Nitrogen API Reference".
headline() -> "<a name=top></a>Nitrogen API Reference".

hlink(Title, Name) -> [
    #p{},
    #hr{},
    #link { text="(back to top)", url="#top", style="float: right;" },
    wf:f("<a name=~s><h2>~s</h2></a>", [Name, Title])
].

body() -> [
    #link { text="Convenience Functions", url="#convenience_functions" }, #br{},
    #link { text="AJAX Updates", url="#ajax_updates" }, #br{},
    #link { text="Event Wiring", url="#event_wiring" }, #br{},
    #link { text="Comet", url="#comet" }, #br{},
    #link { text="Continuations/Polling", url="#continuations" }, #br{},
    #link { text="Redirects", url="#redirect" }, #br{},
    #link { text="Session State", url="#session_state" }, #br{},
    #link { text="Page State", url="#page_state" },  #br{},
    #link { text="Authentication and Authorization", url="#authentication" }, #br{},
    #link { text="HTTP Request and Response", url="#request_and_response" }, #br{},
    #link { text="Cookies", url="#cookies" }, #br{},
    #link { text="Headers", url="#headers" }, #br{},
    #link { text="Serialization", url="#serialization" }, #br{},
    #p{},

	hlink("Convenience Functions", convenience_functions),	
	
	#h3 { text="wf:f(Format, Data) -> String" },
	"Convenience function to format a string similar to io_lib:format(Format, Data). Returns a flattened list.",
	
	#h3 { text="wf:coalesce([List]) -> Item" },
	"Returns the first element in the list that is not 'undefined'.",
	
	#h3 { text="wf:is_string(Term) -> Bool" },
	"Returns true if the Term is an Erlang string. That is, a flat list of integers.",
	
	#h3 { text="wf:to_list(Term) -> List" },
	"Converts the supplied term to a flat list, if possible. Useful for turning Integers, Atoms, Binaries into Strings.",
	
	#h3 { text="wf:to_atom(Term) -> Atom" },
	"Converts the supplied term into an Atm, if possible. Useful for turning Integers, Binaries, and Strings into Atoms.",

	#h3 { text="wf:to_binary(Term) -> Binary" },
	"Converts the supplied term into a Binary, if possible. Useful for turning Integers, Atoms, and Strings into Binaries.",

	#h3 { text="wf:to_integer(Term) -> Integer" },
	"Converts the supplied term into an Integer, if possible. Useful for turning Atoms, Strings, and Binaries into Integers.",

	#h3 { text="wf:html_encode(String) -> EncodedString" },
	"HTML encodes the supplied String, converting things like &lt; and &gt; into &amp;lt; and &amp;gt;.",
	
	#h3 { text="wf:guid() -> String" },
	"Returns a guid. That is, highly unique 16 byte value, represented as a hex string 32 characters long.",
	
    #h3 { text="wf:temp_id() -> String" },
    "
        Return a temp id. Useful for naming an Element so that you can refer to it during a 
        postback later, without giving it a specific name.
    ",
    
    
    
	hlink("AJAX Updates", ajax_updates),
	
	#h3 { text="wf:update(TargetID, Elements) -> ok" },
	"Replace the contents of TargetID with a new set of Nitrogen Elements.",
	
	#h3 { text="wf:insert_top(TargetID, Elements) -> ok" },
	"Insert Nitrogen Elements at the top of TargetID, shifting the existing contents downward.",
	
	#h3 { text="wf:insert_bottom(TargetID, Elements) -> ok" },
	"Insert Nitrogen Elements at the bottom of the TargetID, below the existing contents.",

    #h3 { text="wf:flash(Elements) -> ok" },
    "Insert the Nitrogen Elements as a new flash message.",


	hlink("Event Wiring", event_wiring),
	
	#h3 { text="wf:wire(Actions) -> ok" },
	"
	    Wire actions to the page. The Actions are applied against the entire page unless a
	    trigger or target are specified within the action itself.<br>
	    For example, show a Javascript alert:<br>
	    <code>wf:wire(#alert { text=\"Hello, World!\" })</code>
	",
	
	#h3 { text="wf:wire(TargetID, Actions) -> ok" },
	"
	    Wire actions to the page, targeted against supplied TargetID.<br>
	    For example, hide a Panel:<br>
	    <code>wf:wire(PanelID, #hide {})</code>
	",
	
	#h3 { text="wf:wire(TriggerID, TargetID, Actions) -> ok" },
	"
	    Wire actions to the page, triggering on the supplied TriggerID and targeting against
	    the supplied TargetID. This allows you to wire actions (such as #event) that listen
	    to a click on one element and modify a different element.<br>
	    For example, when a button is clicked, hide a panel:<br>
	    <code>wf:wire(ButtonID, PanelID, #event { type=click, actions=#hide {} })</code>
	",


	hlink("Comet", comet),
	#h3 { text="wf:comet(Function) -> Pid" },
	"
	    Spawn a function and tell the browser to open a COMET request to receive the results in real time.<br>
        See <a href='/web/samples/comet1'>example 1</a>, <a href='/web/samples/comet2'>example 2</a>, and
        <a href='/web/samples/comet3'>example 3</a> for usage.
	",
	
	#h3 { text="wf:comet_flush() -> ok" },
	"
	    Normally, the results of a comet function are sent to the browser when the function exits.
	    comet_flush/0 pushes results to the browser immediately, useful for a looping comet function.
	",
	
	
	 
	 hlink("Continuations", continuations),
	 
	 #h3 { text="wf:continue(Tag, Function) -> ok" },
	 "
	    Spawn the provided function (arity 0) and tell the browser to poll for the results.<br>
	    See <a href='/web/samples/continuations'>continuations example</a> for usage.
	",
	
	 #h3 { text="wf:continue(Tag, Function, Interval) -> ok" },
	 "
	    Spawn the provided function (arity 0) and tell the browser to poll for the results at the specified interval.<br>
	    See <a href='/web/samples/continuations'>continuations example</a> for usage.
	",

	 #h3 { text="wf:continue(Tag, Function, IntervalInMS, TimeoutInMS) -> ok" },
	 "
	    Spawn the provided function (arity 0) and tell the browser to poll for the results at the specified interval, with a timeout setting.<br>
	    See <a href='/web/samples/continuations'>continuations example</a> for usage.
	",
	
	
	

	hlink("Redirect", redirect),
	
	#h3 { text="wf:redirect(Url) -> ok" },
	"Redirect to the provided URL.",
	
	#h3 { text="wf:redirect_to_login(Url) -> ok" },
	"
	    Redirect to the provided URL, attaching a token on the end. The recieving page can call
	    <code>wf:redirect_from_login(DefaultUrl)</code> to send the user back to the current page.
	",
	
	#h3 { text="wf:redirect_from_login(DefaultUrl) -> ok" },
	"
	    Redirect the user back to a page that called <code>wf:redirect_to_login(Url)</code>. If 
	    the user came to the page for some other reason, then the user is redirected to the 
	    provided DefaultUrl.
	",
	
	hlink("Session State", session_state),

	#h3 { text="wf:session(Key) -> Value or 'undefined'" },
	"
	    Retrieve the session value stored under the specified key.<br>
	    For example, retrieve the value of 'count' for the current user:<br>
	    <code>Count = wf:session(count)</code>
	",
	
	#h3 { text="wf:session(Key, Value) -> ok" },
	" 
	    Store a session variable for the current user. Key and Value can be any Erlang term.<br>
	    For example, store a count:<br>
	    <code>wf:session(count, Count)</code>
	",
	
	#h3 { text="wf:clear_session() -> ok" },
	"
	    Clear the current user's session.
	",
	
	#h3 { text="wf:logout() -> ok" },
	"Clear session state, page state, identity, and roles.",	
	
	
	
	hlink("Page State", page_state),
	
	#h3 { text="wf:state(Key) -> Value" },
	"
	    Retrieve a page state value stored under the specified key. Page State is
	    different from Session State in that Page State is scoped to a series
	    of requests by one user to one Nitrogen Page.
	",
	
	#h3 { text="wf:state(Key, Value) -> ok" },
	"
	    Store a page state variable for the current user. Page State is
	    different from Session State in that Page State is scoped to a series
	    of requests by one user to one Nitrogen Page.
	",
	
	#h3 { text="wf:clear_state() -> ok" },
	"Clear a user's page state.",
	
	
	hlink("Authentication and Authorization", authentication),
	
	#h3 { text="wf:user() -> User or 'undefined'" },
	"Return the user value that was previously set by <code>wf:user(User)</code>",

	#h3 { text="wf:user(User) -> ok" },
	"Set the user for the current session.",
	
	#h3 { text="wf:clear_user() -> ok" },
	"Same as <code>wf:user(undefined)</code>.",
	
	#h3 { text="wf:role(Role) -> 'true' or 'false'" },
	"Check if the current user has a specified role.",
	    
	#h3 { text="wf:role(Role, IsInRole) -> ok" },
	"Set whether the current user is in a specified role.",
	
	#h3 { text="wf:clear_roles() -> ok" },
	"Remove the user from all roles.",	


	
	hlink("Web Request and Response", request_and_response),
	
	#h3 { text="wf:q(AtomKey) -> [StringValue]" },
	"
	    Get all query string and POST values for the provided key. In most cases,
	    returns a list of Values, though in most cases it will be a list of length 1.
	",
		
	#h3 { text="wf:set_response_code(IntegerCode) -> ok" },
	"Set the HTTP response code. Defaults to 200",
	
	#h3 { text="wf:set_content_type(ContentType) -> ok" },
	"
		Set the HTTP content type. Defaults to \"text/html\". This can be
		used to return text images or other files to the browser, rather than returning 
		HTML.
	",

	#h3 { text="wf:get_path_info() -> String" },
	"
		Return the path info for the requested page. In other words, if the module
		web_my_page is requsted with the path \"/web/my/page/some/extra/stuff\", then
		wf:get_path_info() would return \"some/extra/stuff\".
	",
	
	#h3 { text="wf:get_page_module() -> Atom" },
	"
		Returns the requested page module. Useful information to know when writing a custom
	 	element or action.
	",
	
	
	
	hlink("HTTP Cookies", cookies),

	#h3 { text="wf:set_cookie(Key, Value) -> ok" },
	"
		Tell Nitrogen to set a cookie on the browser. Uses \"/\" for the Path, and Nitrogen's
		session timeout setting for the MinutesToLive value.
	",
	
	#h3 { text="wf:set_cookie(Key, Value, Path, MinutesToLive) -> ok" },
	"
		Tell Nitrogen to set a cookie on the browser under the specified Path that is valid
		for a certain number of minutes.
	",
	
	#h3 { text="wf:get_cookie(Key) -> String" },
	"Retrieve a previously set cookie.",
	

	
	hlink("HTTP Headers", headers),
	
	#h3 { text="wf:get_headers() -> [{AtomKey, StringValue}, ...]" },
	"Returns a proplist of all HTTP headers.",
	
	#h3 { text="wf:get_header(AtomKey) -> Value" },
	"Returns the value of an HTTP header.",
	
	#h3 { text="wf:set_header(StringKey, HeaderValue) -> ok" },
	"Sets an HTTP header during the next response.",
	
	
	
	hlink("Serialization", serialization),
	
	#h3 { text="wf:pickle(Term) -> PickledBinary" },
	"Serialize a term into a web-safe hex string, with checksumming. (Not encrypted!)",
	
	#h3 { text="wf:depickle(PickledBinary) -> Term" },
	"Turn a pickled binary back into the original term.",
	
	#h3 { text="wf:depickle(PickledBinary, SecondsToLive) -> {IsStillValid, Term}" },
	"
		Turn a pickled binary back into the original term, checking to see if the term was 
		pickled more than SecondsToLive second ago. Returns a tuple indicating if the term 
		is still 'fresh'.
	"
].
