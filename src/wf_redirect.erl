-module (wf_redirect).
-export ([
	redirect/1,
	redirect_to_login/1,
	redirect_from_login/1
]).

redirect(Url) -> 
	put(is_redirect, true),
	wf:wire(wf:f("document.location.href=\"~s\";", [wf_utils:js_escape(wf:to_list(Url))])).

redirect_to_login(Url) ->
	Req = get(mochiweb_request),
	RawPath = Req:get(raw_path),
	NewUrl = web_x:link(Url, ?MODULE, redirect_to_login_callback, RawPath),
	case Req:get(method) of
		'GET' -> 
			{redirect, NewUrl};
		
		'POST' -> 
			Script = wf:wire(wf:f("document.location.href=\"~s\";", [wf_utils:js_escape(wf:to_list(NewUrl))])),
			{content, "application/javascript", Script}
	end.
	
redirect_from_login(DefaultPath) ->	
	% Redirect to the page the user was
	% originally trying to navigate to.
	% If that page isn't aronud, then redirect to 
	% DefaultPath.
  case web_x:go(redirect_to_login_callback) of
		not_handled -> wf:redirect(DefaultPath);
		{ok, _ } -> ignore
	end.

redirect_to_login_callback(_, Path) ->
	wf:redirect(Path).