-module (web_learn_screencasts).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', screencasts}
]}.

title() -> "Nitrogen Screencasts".
headline() -> "Nitrogen Screencasts".

body() -> [
	#h3 { text="Nitrogen December 2008 New Features (.mov)" },
	#p{},
	"
		<a target='_self' href='/NitrogenDec2008.html'>
		<img alt='View Movie' src='/images/NitrogenDec2008.png' align='middle' height='215' width='320'/>
		</a>
	",
	#hr{},
	#h3 { text="Flickr on Nitrogen (.mov)" },
	#p{},
	"
		<a target='_self' href='/FlickrOnNitrogen.html'>
		<img alt='View Movie' src='/images/FlickrOnNitrogen.png' align='middle' height='235' width='320'/>
		</a>
	"
].

event(_) -> ok.
