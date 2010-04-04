-module (domes_contenttype).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Set Content Type".
headline() -> "Set Content Type".
right() -> linecount:render().

body() -> [
	"
		You can use Nitrogen to dynamically serve content other than
		HTML. The image below is served as raw data from a Nitrogen module. 
	", 
	#p{},
	#image { image="/web/samples/contenttype/image" },
	#p{},
	"View the <a href=viewsource?module=demos_contenttype_image>source code</a> of demos_contenttype_image.erl to see how it's done."
	
].
	
event(_) -> ok.
