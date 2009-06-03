% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_redirect).
-include ("wf.inc").
-export ([
	redirect/2% ,
	% 	redirect_to_login/1,
	% 	redirect_from_login/1,
	% 	redirect_to_login_callback/2
]).

redirect(Url, Context) -> 
	wff:wire(#redirect { url=Url }, Context).
% 
% redirect_to_login(Url, Context) ->
% 	RawPath = wf_platform:get_raw_path(),
% 	NewUrl = web_x:link(Url, ?MODULE, redirect_to_login_callback, RawPath),
% 	redirect(NewUrl, Context).
% 	
% redirect_from_login(DefaultPath, Context) ->	
% 	% Redirect to the page the user was
% 	% originally trying to navigate to.
% 	% If the user came straight to the login page, 
% 	% then redirect to DefaultPath.
%   case web_x:go(redirect_to_login_callback) of
% 		not_handled -> redirect(DefaultPath, Context);
% 		{ok, _ } -> ignore
% 	end.
% 
% redirect_to_login_callback(_, Path) -> redirect(Path).