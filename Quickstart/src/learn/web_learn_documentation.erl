-module (web_learn_documentation).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', documentation}
]}.

title() -> "Nitrogen Documentation".
headline() -> "Nitrogen Documentation".

body() -> [
	#link { text="Nitrogen Wiki on GitHub", url="http://github.com/rklophaus/nitrogen/wikis" },
	#link { text="5 Minute Blog Using Nitrogen and CouchDB", url="http://medevyoujane.com/blog/2008/12/12/5-minute-blog-using-nitrogen-and-couchdb.html"}
].

event(_) -> ok.
