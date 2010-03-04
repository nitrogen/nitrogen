-module (web_download).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/onecolumn.html", bindings=[
	{'Group', code},
	{'Item', download}
]}.

title() -> "Download".
headline() -> "Download".

body() -> [
	#p{},
	#h3 { text="Get the Latest Code" },
	#p{},
	#link { text="Download the latest code from GitHub", url="http://github.com/rklophaus/nitrogen/" },
	
	 
	#h3 { text="Track Bugs" },
	#p{},
	#link { text="File and track bugs on Lighthouse", url="http://nitrogen.lighthouseapp.com/dashboard" }

].
