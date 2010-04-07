-module(common).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

header(Selected) ->
    wf:wire(Selected, #add_class { class=selected }),
    #panel { class=menu, body=[
        #link { id=home, url='/', text="HOME" },
        #link { id=downloads, url='/downloads', text="DOWNLOADS" },
        #link { id=demos, url='/demos', text="DEMOS" },
        #link { id=learn, url='/learn', text="LEARN MORE" },
        #link { id=community, url='/community', text="GET INVOLVED" }
    ]}.


footer() ->
    #panel { class=credits, body=[
        "
        Copyright &copy; 2008-2010 <a href='http://rklophaus.com'>Rusty Klophaus</a>. 
        <img src='/images/MiniSpaceman.png' style='vertical-align: middle;' />
        Released under the MIT License.
        "
    ]}.

