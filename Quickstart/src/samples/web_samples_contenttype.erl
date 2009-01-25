-module (web_samples_contenttype).
-include ("wf.inc").
-include ("google_chart.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
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
	"View the <a href=viewsource?module=web_samples_contenttype_image>source code</a> of web_samples_contenttype_image.erl to see how it's done."
	
].
	
event(_) -> ok.