% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (log_bridge).
-export ([
	behaviour_info/1
]).

behaviour_info(callbacks) -> [
	% init(Context) -> ParameterizedModule
	% Called at the start of the request.
	{init, 1},      

	% render(Context) -> NewContext
	% Called at the end of the request, before sending
	% a response back to the browser.
	{render, 1},
	
	% info(String) -> ParameterizedModule
	% Log an info-level message. Everything is functioning as usual.
	{info, 1},       
	
	% warning(String) -> ParameterizedModule
	% Log a warning-level message. If something is not corrected, then
	% service could be interrupted in some way.
	{warning, 1},
	
	% error(Key) -> ParameterizedModule
	% Log an error-level message. Service has been interrupted in some way.
	{error, 1}
];

behaviour_info(_) -> undefined.
