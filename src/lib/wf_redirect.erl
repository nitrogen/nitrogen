% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_redirect).
-export ([
	redirect/1,
	redirect_to_login/1,
	redirect_from_login/1,
	redirect_to_login_callback/2
]).

redirect(Url) -> wf_platform:set_redirect(Url).

redirect_to_login(Url) ->
	RawPath = wf_platform:get_raw_path(),
	NewUrl = web_x:link(Url, ?MODULE, redirect_to_login_callback, RawPath),
	redirect(NewUrl).
	
redirect_from_login(DefaultPath) ->	
	% Redirect to the page the user was
	% originally trying to navigate to.
	% If the user came straight to the login page, 
	% then redirect to DefaultPath.
  case web_x:go(redirect_to_login_callback) of
		not_handled -> redirect(DefaultPath);
		{ok, _ } -> ignore
	end.

redirect_to_login_callback(_, Path) -> redirect(Path).