-module (web_discuss_community).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/onecolumn.html", bindings=[
	{'Group', discuss},
	{'Item', community}
]}.

title() -> "Community".
headline() -> "Community".

body() -> [
	#h2 { text="Newsgroup / Mailing List" },
	#p{},
	#link { text="Nitrogen Web Framework Group", url="http://groups.google.com/group/nitrogenweb" },
	
	#h2 { text="IRC" },
	#p{},
	"#nitrogen on irc.freenode.net"
].
