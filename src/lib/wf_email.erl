% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_email).
-include ("wf.inc").
-export ([send/4]).

send(From, To, Subject, Body) ->
	From1 = wf:to_list(From),
	
	To1 = case is_list(hd(To)) of
		true -> [wf:to_list(X) || X <- To];
		false -> [wf:to_list(To)]
	end,
	Subject1 = wf:to_list(Subject),
	Body1 = wf:to_list(Body),
	
	Port = open_port({spawn,"./bin/mailscript"}, [stream,use_stdio]),
	true = port_command(Port, "From: " ++ From1 ++ "\n"),
	[true = port_command(Port, "To: " ++ X ++ "\n") || X <- To1],
	true = port_command(Port, "Subject: " ++ Subject1 ++ "\n"),
	true = port_command(Port, "Content-Type: text/html; charset=\"us-ascii\"" ++ "\n"),
	true = port_command(Port, "\n"),
	true = port_command(Port, Body1 ++ "\n"),
	true = port_command(Port, "." ++ "\n"),
	true = port_close(Port).
