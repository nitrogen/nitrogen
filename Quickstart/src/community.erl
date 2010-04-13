-module (community).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/grid.html" }.

title() -> "Get Involved".

layout() -> 
    #container_12 { body=[
        #grid_12 { alpha=true, omega=true, class=header, body=common:header(community) },
        #grid_clear {},

        #grid_10 { alpha=true, omega=true, prefix=1, suffix=1, class=headline, body=headline() },
        #grid_clear {},

        #grid_10 { alpha=true, prefix=1, suffix=1, omega=1, body=top() },

        #grid_10 { alpha=true, prefix=1, suffix=1, omega=1, body=articles() },
        #grid_clear {},

        #grid_12 { alpha=true, omega=true, body=common:footer() }
    ]}.

headline() -> 
    "Get Involved".

top() -> 
    [
        #grid_10 { alpha=true, omega=true, body=[
            #hr {}
        ]},
        #grid_clear{},

        #grid_3 { alpha=true, body=[
            #h2 { text="Newsgroup" },
        
            #p{},
            "
            Subscribe to the <a href='http://groups.google.com/group/nitrogenweb'>Nitrogen Google Group</a>.
            "
        ]},

        #grid_2 { body=[
            #h2 { text="IRC" },
            
            #p{},
            "
            Join <code>#nitrogen</code> on irc.freenode.net.
            "
        ]},

        #grid_2 { body=[
            #h2 { text="Bugs" },
            
            #p{},
            "
            Submit feature requests and file bugs at <a href='http://nitrogen.lighthouseapp.com'>Lighthouse</a>.
            "
        ]},

        #grid_3 { omega=true, body=[
            #h2 { text="Contribute Code" },
            
            #p{},
            "
            Fork Nitrogen on <a href='http://github.com/rklophaus/nitrogen'>GitHub</a>.
            "
        ]},

        #grid_clear {},

        #grid_10 { alpha=true, omega=true, body=[
            #hr {}
        ]},

        #grid_clear {},
        
        #grid_10 { alpha=true, omega=true, body=[
            #h2 { text="Donate" },
            
            #p{},
            "
            Do you use Nitrogen? Help fund Nitrogen development.
            <p>
            <a href='http://www.pledgie.com/campaigns/2057'><img alt='Click here to lend your support to: nitrogen and make a donation at www.pledgie.com !' src='http://www.pledgie.com/campaigns/2057.png?skin_name=chrome' border='0' /></a>
            <p>
            Alternatively, show your appreciation by sending books, t-shirts, mugs, gadgets, and other shwag to:
            <p>
            Basho Technologies / Rusty Klophaus / 196 Broadway / Cambridge, MA 02139
            "
        ]},

        #grid_clear{},
        
        #grid_10 { alpha=true, omega=true, body=[
            #hr {}
        ]}
    ].

articles() ->
    [
        #h1 { text="Articles" },

        #h2 { text="March 2010" },
        
        #p{},
        #link { text="Erlang, Nitrogen and automatic rebuild. (objitsu.com)", url="http://objitsu.com/blog/2010/03/16/erlang-nitrogen-and-automatic-rebuild/" },

        #h2 { text="December 2009" },
        
        #p{},
        #link { text="Erlang Web Development with Nitrogen (sergioveiga.com)", url="http://sergioveiga.com/index.php/2009/12/03/erlang-web-development-with-nitrogen/" },

        #h2 { text="October 2009" },

        #p{},
        "Tristan Sloughter's Nitrogen Presentation at Chicago Erlang User's Group - ",
        #link { text="Part 1", url="http://chicagoerlangusergroup.blogspot.com/2009/10/video-of-tristans-nitrogen-talk-part-1.html" }, ", ",
        #link { text="Part 2", url="http://chicagoerlangusergroup.blogspot.com/2009/10/video-of-tristans-nitrogen-talk-part-2.html" },

        #p{},
        #link { text="redhoterlang.com and the use of Nitrogen", url="http://www.redhoterlang.com/web/plink?id=10807f2903db06ceab888e4b163d675a" },

        #h2 { text="July 2009" },

        #p{},
        #link { text="Build Your Next Web Application with Erlang", url="http://steve.vinoski.net/pdf/IC-Build_Your_Next_Web_Application_with_Erlang.pdf" },
        " - IEEE Internet Computing Magazine, Steve.Vinoski.net",

        #p{},
        #link { text="Using DTL Templates with Nitrogen (fiatdev.com)", url="http://fiatdev.com/2009/07/13/using-dtl-templates-with-nitrogen" },

        #h2 { text="June 2009" },

        #p{},
        #link { text="Advanced Nitrogen Elements (jeremy.marzhissstudios.com)", url="http://jeremy.marzhillstudios.com/index.php/site-news/advanced-nitrogen-elements/" },

        #p{},
        #link { text="Rusty presenting Nitrogen at Erlang Factory in London", url="http://www.erlang-factory.com/conference/London2009/speakers/RustyKlophaus" },
        " - Erlang-Factory.com",

        #p{},
        #link { text="Embedded Webapp on Freerunner (blondon.fr)", url="http://blondon.fr/blog/index.php?post/2009/06/07/freerunner-embedded-webapp" },

        #h2 { text="May 2009" },

        #p{},
        #link { text="Creating Custom Nitrogen Elements (jeremy.marzhissstudios.com)", url="http://jeremy.marzhillstudios.com/index.php/site-news/creating-custom-nitrogen-elements/" },

        #p{},
        #link { text="Erlang Factory 2009 Review (sauria.com)", url="http://www.sauria.com/blog/2009/05/04/erlang-factory-2009/" },

        #h2 { text="April 2009" },

        #p{},
        #link { text="Nitrogen Presentation by Ngoc Dao", url="http://www.slideshare.net/ngocdaothanh/nitrogen-web-framework" },


        #p{},
        #link { text="Rusty presenting Nitrogen at Erlang Factory in San Francisco", url="http://www.erlang-factory.com/conference/SFBayAreaErlangFactory2009/speakers/RustyKlophaus" },
        " - Erlang-Factory.com",

        #p{},
        #link { text="Seethrough and Nitrogen", url="http://www.redhoterlang.com/web/plink?id=bce1408f0f211d3e4951f972b6e9bdbf" },
        " - RedHotErlang.com",  

        #h2 { text="January 2009" },

        #p{},
        #link { text="A Simple Web App using Nitrogen (joeandmotorboat.com)", url="http://www.joeandmotorboat.com/2009/01/08/a-simple-web-app-using-nitrogen/" },

        #h2 { text="December 2008" },
        #p{},
        #link { text="Erlang Web Development Frameworks (medevyoujane.com)", url="http://medevyoujane.com/blog/2008/12/18/erlang-web-development-frameworks.html" },

        #p{},
        #link { text="5 Minute Blog Using Nitrogen and CouchDB (medevyoujane.com)", url="http://medevyoujane.com/blog/2008/12/12/5-minute-blog-using-nitrogen-and-couchdb.html" },

        #p{},
        #link { text="Nitrogen (fiatdev.com)", url="http://fiatdev.com/2008/12/06/nitrogen" }, 

        #h2 { text="November 2008" },
        #p{},
        #link { text="Release of Nitrogen web framework (siteduzero.com)", url="http://translate.google.com/translate?u=http%3A%2F%2Fwww.siteduzero.com%2Fnews-62-30656-sortie-du-framework-web-nitrogen.html&hl=en&ie=UTF-8&sl=fr&tl=en" }, ", ",
        #link { text="French (orig.)", url="http://www.siteduzero.com/news-62-30656-sortie-du-framework-web-nitrogen.html" },

        #h2 { text="October 2008" },
        #link { text="Interview on Nitrogen Web Framework (erlanginside.com)", url="http://erlanginside.com/interview-with-rusty-klophaus-on-the-nitrogen-erlang-web-framework-37" },

        #p{},

        #h1 { text="Projects on Github" },

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

        #p{},
        "Wrote an article that you would like to add to this list? <a href='mailto:nitrogenframework@gmail.com'>Email Rusty</a>."
    ].

