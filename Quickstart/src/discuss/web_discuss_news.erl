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

    #h3 { text="October 2009" },

    #p{},
    "Tristan Sloughter's Nitrogen Presentation at Chicago Erlang User's Group - ",
    #link { text="Part 1", url="http://chicagoerlangusergroup.blogspot.com/2009/10/video-of-tristans-nitrogen-talk-part-1.html" }, ", ",
    #link { text="Part 2", url="http://chicagoerlangusergroup.blogspot.com/2009/10/video-of-tristans-nitrogen-talk-part-2.html" },

    #p{},
    #link { text="redhoterlang.com and the use of Nitrogen", url="http://www.redhoterlang.com/web/plink?id=10807f2903db06ceab888e4b163d675a" },

    #h3 { text="July 2009" },

    #p{},
    #link { text="Build Your Next Web Application with Erlang", url="http://steve.vinoski.net/pdf/IC-Build_Your_Next_Web_Application_with_Erlang.pdf" },
    " - IEEE Internet Computing Magazine, Steve.Vinoski.net",

    #p{},
    #link { text="Using DTL Templates with Nitrogen", url="http://fiatdev.com/2009/07/13/using-dtl-templates-with-nitrogen" },
    " - FiatDev.com",

    #h3 { text="June 2009" },

    #p{},
    #link { text="Advanced Nitrogen Elements", url="http://jeremy.marzhillstudios.com/index.php/site-news/advanced-nitrogen-elements/" },
    " - Jeremy.Marzhillstudios.com",

    #p{},
    #link { text="Rusty presenting Nitrogen at Erlang Factory in London", url="http://www.erlang-factory.com/conference/London2009/speakers/RustyKlophaus" },
    " - Erlang-Factory.com",

    #p{},
    #link { text="Embedded Webapp on Freerunner", url="http://blondon.fr/blog/index.php?post/2009/06/07/freerunner-embedded-webapp" },
    " - blondon.fr",	

    #h3 { text="May 2009" },

    #p{},
    #link { text="Creating Custom Nitrogen Elements", url="http://jeremy.marzhillstudios.com/index.php/site-news/creating-custom-nitrogen-elements/" },
    " - Jeremy.Marzhillstudios.com",

    #p{},
    #link { text="Erlang Factory 2009 Review", url="http://www.sauria.com/blog/2009/05/04/erlang-factory-2009/" },
    " - www.sauria.com, Ted Leung",    

    #h3 { text="April 2009" },

    #p{},
    #link { text="Nitrogen Presentation by Ngoc Dao", url="http://www.slideshare.net/ngocdaothanh/nitrogen-web-framework" },


    #p{},
    #link { text="Rusty presenting Nitrogen at Erlang Factory in San Francisco", url="http://www.erlang-factory.com/conference/SFBayAreaErlangFactory2009/speakers/RustyKlophaus" },
    " - Erlang-Factory.com",

    #p{},
    #link { text="Seethrough and Nitrogen", url="http://www.redhoterlang.com/web/plink?id=bce1408f0f211d3e4951f972b6e9bdbf" },
    " - RedHotErlang.com",	

    #p{},
    #h3 { text="January 2009" },

    #p{},
    #link { text="A Simple Web App using Nitrogen", url="http://www.joeandmotorboat.com/2009/01/08/a-simple-web-app-using-nitrogen/" },
    " - JoeAndMotorboat.com",

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

    #h2 { text="Websites running Nitrogen" },

    #p{},
    #link { text="Opinion8r.com", url="http://opinion8r.com" },
    " - rate. review. everything!",

    #p{},
    #link { text="ZincBB", url="http://open.cetiforge.com/" },
    " - Online message board powered by ", #link { text="ZincBB", url="http://github.com/TomMc/ZincBB" },

    #p{},
    #link { text="RedHotErlangBlog.com", url="http://www.redhoterlang.com/" },

    #h2 { text="Projects on Github" },

    #p{},
    #link { text="http://github.com/kungfooguru/beerenthusiasts", url="http://github.com/kungfooguru/beerenthusiasts" },
    " - Homebrewing and beer lovers webapp written in Erlang and using Nitrogen, Couchdb and Mnesia.",

    #p{},
    #link { text="http://github.com/Joony/erlang-nitrogen-user-login", url="http://github.com/Joony/erlang-nitrogen-user-login" },
    " - A simple user login example.",

    #p{},
    #link { text="http://github.com/baphled/chatterl_nitrogen/", url="http://github.com/baphled/chatterl_nitrogen/" },
    " - Chatterl web frontend using Nitrogen",

    #p{},
    #link { text="http://github.com/marksands/Nitro-To-Do/", url="http://github.com/marksands/Nitro-To-Do/" },
    " - Basic To-Do list app in Nitrogen",

    #h2 { text="More Buzz" },
    #p{},
    #link { text="Nitrogen on Twitter", url="http://search.twitter.com/search?q=&ands=nitrogen&phrase=&ors=web+erlang+%23nitrogen&nots=&tag=&lang=all&from=&to=&ref=&near=&within=15&units=mi&since=&until=&rpp=50" },
    #p{},
    #link { text="Nitrogen on Reddit", url="http://www.reddit.com/search?q=nitrogen+erlang&sort=new" }
].
