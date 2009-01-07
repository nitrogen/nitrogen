% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_body).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, body).

render(_ControlID, Record) ->
	["
	<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\">
	<html>
	<head>
	<title>
	", Record#body.title, "
	</title>	
	<script src='/nitrogen/jquery-1.2.6.js' type='text/javascript' charset='utf-8'></script>
	<script src='/nitrogen/jquery-ui-personalized-1.5.3.min.js' type='text/javascript' charset='utf-8'></script>
	<script src='/nitrogen/livevalidation.js' type='text/javascript' charset='utf-8'></script>
	<script src='/nitrogen/nitrogen.js' type='text/javascript' charset='utf-8'></script>
	<link href='/nitrogen/nitrogen.css' type='text/css' rel='stylesheet'>
	<body>
	<form id='page' onsubmit='return false;'>
	", wf:render(Record#body.body), "
	<script>
	", script, "
	</script>
	</body>
	</html>
	"].
									
								
		
				