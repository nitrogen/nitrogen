-module (web_discuss_news).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/onecolumn.html", bindings=[
	{'Group', discuss},
	{'Item', news}
]}.

title() -> "Latest News".
headline() -> "Latest News".

body() -> [
	#p{},
	"Wrote an article about Nitrogen? <a href='mailto:nitrogenframework@gmail.com'>Let us know</a>.",

	#h2 { text="Articles" },
	
	#h3 { text="December 2008" },
	#p{},
	#link { text="Erlang Web Development Frameworks", url="http://medevyoujane.com/blog/2008/12/18/erlang-web-development-frameworks.html" },
	" - MeDevYouJane.com",
		
	#p{},
	#link { text="5 Minute Blog Using Nitrogen and CouchDB", url="http://medevyoujane.com/blog/2008/12/12/5-minute-blog-using-nitrogen-and-couchdb.html" },
	" - MeDevYouJane.com",
	
	#p{},
	#link { text="Nitrogen", url="http://fiatdev.com/2008/12/06/nitrogen" }, 
	" - FiatDevelopment",

	
	#h3 { text="November 2008" },
	#p{},
	#link { text="Release of Nitrogen web framework", url="http://translate.google.com/translate?u=http%3A%2F%2Fwww.siteduzero.com%2Fnews-62-30656-sortie-du-framework-web-nitrogen.html&hl=en&ie=UTF-8&sl=fr&tl=en" }, ", ",
	#link { text="French (orig.)", url="http://www.siteduzero.com/news-62-30656-sortie-du-framework-web-nitrogen.html" },
	" - SiteDuZero.com",
	
	#h3 { text="October 2008" },
	#link { text="Interview on Nitrogen Web Framework", url="http://erlanginside.com/interview-with-rusty-klophaus-on-the-nitrogen-erlang-web-framework-37" },
	" - ErlangInside.com",
	
	#p{},
	
	#h2 { text="More Buzz" },
	#p{},
	#link { text="Nitrogen on Twitter", url="http://search.twitter.com/search?q=&ands=nitrogen&phrase=&ors=web+erlang+%23nitrogen&nots=&tag=&lang=all&from=&to=&ref=&near=&within=15&units=mi&since=&until=&rpp=50" },
	#p{},
	#link { text="Nitrogen on Reddit", url="http://www.reddit.com/search?q=nitrogen+erlang&sort=new" }
].
