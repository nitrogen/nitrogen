% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_redirect).
-include ("wf.inc").
-compile(export_all).

render_action(Record) ->
	DestinationUrl = Record#redirect.url,
	wf:f("window.location=\"~s\";", [wf:js_escape(DestinationUrl)]).
	
	
redirect(Url) -> 
	wf:wire(#redirect { url=Url }).
