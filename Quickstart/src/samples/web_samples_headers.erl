-module (web_samples_headers).
-include ("wf.inc").
-include ("google_chart.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Headers".
headline() -> "Headers".
right() -> linecount:render().

body() -> 
	?PRINT(wf:get_headers()),
	?PRINT(wf:get_header(authorization)),
	Headers = [{wf:to_list(Key), wf:to_list(Value)} || {Key, Value} <- wf:get_headers()],

	[
		#table { rows=[
			#tablerow { cells=[
				#tableheader { text="Header", style="width: 120px; background-color: #ddd;" },
				#tableheader { text="Value", style="background-color: #ddd;" }
			]},
			#bind {
				data=Headers,
				map={headerCell@text, valueCell@text},
				body=#tablerow { cells=[
					#tablecell { id=headerCell, style="background-color: #eee; font-size: .8em;" }, 
					#tablecell { id=valueCell, style="font-size: .8em;" }
				]}
			}
		]}
	].
	
event(_) -> ok.