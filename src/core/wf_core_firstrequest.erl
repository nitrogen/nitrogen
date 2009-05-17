-module (nitrogen_core_firstrequest).
-include ("simplebridge.hrl").
-export ([run/3]).

% run/4 
% Responds with { HTML, NewContext }

run(Module, Context) ->
	% Run the request and get the response body...
	Module:event(load, Context).