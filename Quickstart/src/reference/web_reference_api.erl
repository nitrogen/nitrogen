-module (web_reference_api).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', reference},
	{'Item', api}
]}.

title() -> "Nitrogen API Reference".
headline() -> "Nitrogen API Reference".

body() -> [
	#h2 { text="Convenience Functions" },
	
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
	"Returns a guid. That is, highly unique 32 byte hex string.",
	
	
	
	#h2 { text="Serialization" },
	
	#h3 { text="wf:pickle(Term) -> PickledBinary" },
	"Serialize a term into a web-safe hex string, with checksumming. (Not encrypted!)",
	
	#h3 { text="wf:depickle(PickledBinary) -> Term" },
	"Turn a pickled binary back into the original term.",
	
	#h3 { text="wf:depickle(PickledBinary, SecondsToLive) -> {IsStillValid, Term}" },
	"
		Turn a pickled binary back into the original term, checking to see if the term was 
		pickled more than SecondsToLive second ago. Returns a tuple indicating if the term 
		is still 'fresh'.
	",
		
	
	
	#h2 { text="Web Request and Response" },
		
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
	
	
	
	#h2 { text="HTTP Cookies" },

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
	

	
	#h2 { text="HTTP Headers" },
	
	#h3 { text="wf:get_headers() -> [{AtomKey, StringValue}, ...]" },
	"Returns a proplist of all HTTP headers.",
	
	#h3 { text="wf:get_header(AtomKey) -> Value" },
	"Returns the value of an HTTP header.",
	
	#h3 { text="wf:set_header(StringKey, HeaderValue) -> ok" },
	"Sets an HTTP header during the next response.",
	


	#h2 { text="AJAX Updates" },
	
	#h3 { text="wf:update(TargetID, Elements) -> ok" },
	"Replace the contents of TargetID with a new set of Nitrogen Elements.",
	
	#h3 { text="wf:insert_top(TargetID, Elements) -> ok" },
	"Insert Nitrogen Elements at the top of TargetID, shifting the existing contents downward.",
	
	#h3 { text="wf:insert_bottom(TargetID, Elements) -> ok" },
	"Insert Nitrogen Elements at the bottom of the TargetID, below the existing contents.",


	#h2 { text="Event Wiring" }
].

% wire(Actions) -> wf_render:wire(Actions).
% wire(TargetID, Actions) -> wf_render:wire(TargetID, Actions).
% wire(TriggerID, TargetID, Actions) -> wf_render:wire(TriggerID, TargetID, Actions).
% 
% 
% %%% WF_CONTINUE %%%
% 
% continue(Tag, Function) -> wf_continuation:continue(Tag, Function).
% continue(Tag, Function, Interval) -> wf_continuation:continue(Tag, Function, Interval).
% continue(Tag, Function, Interval, Timeout) -> wf_continuation:continue(Tag, Function, Interval, Timeout).
% 
% 
% %%% WF_COMET %%%
% 
% comet(Function) -> wf_comet:comet(Function).
% comet_flush() -> wf_comet:comet_flush().
% 
% 
% %%% WF_REDIRECT %%%
% 
% redirect(Url) -> wf_redirect:redirect(Url).
% redirect_to_login(Url) -> wf_redirect:redirect_to_login(Url).
% redirect_from_login(DefaultUrl) -> wf_redirect:redirect_from_login(DefaultUrl).
% 
% 
% 
% %%% WF_SESSION %%%
% 
% state(Key) -> wf_state:state(Key).
% state(Key, Value) -> wf_state:state(Key, Value).
% clear_state() -> wf_state:clear_state().
% 
% 
% %%% WF_STATE %%%
% 
% user() -> wf_session:user().
% user(User) -> wf_session:user(User).
% clear_user() -> wf_session:clear_user().
% 
% role(Role) -> wf_session:role(Role).
% role(Role, IsInRole) -> wf_session:role(Role, IsInRole).
% clear_roles() -> wf_session:clear_roles().
% 
% session(Key) -> wf_session:session(Key).
% session(Key, Value) -> wf_session:session(Key, Value).
% clear_session() -> wf_session:clear_session().
% 
% logout() -> clear_user(), clear_roles(), clear_state(), clear_session().
% 
% 
% %%% WF_CACHE %%%
% 
% cache(Key, Function) -> wf_cache:cache(Key, Function).
% cache(Key, Function, Options) -> wf_cache:cache(Key, Function, Options).
% 
% 
% %%% WF_QUERY %%%
% 
% q(Q) -> wf_query:q(Q).
% 
% 
% %%% WF_PATH %%%
% 
% me_var() -> wf_render:me_var().
% temp_id() -> wf_path:temp_id().
% to_js_id(Path) -> wf_path:to_js_id(Path).
% 
% 
% %%% OTHER %%%
% 
% flash(Terms) -> element_flash:add_flash(Terms).
% 
% assert(true, _) -> ok;
% assert(false, Error) -> erlang:error(Error).
% 
% 
% 
% 
% 
% 
% 
% "Coming soon.".
% 
% event(_) -> ok.
