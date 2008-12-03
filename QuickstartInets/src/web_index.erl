-module (web_index).
-include ("wf.inc").
-export ([main/0, event/1]).

main() ->
	Body = #body { body=#panel { style="margin: 50px;", body=[
		#h1 { text="Nitrogen Example Pages" },
	
		#h3 { text="Controls and Validation" },
		link("Simple Controls", "/web/simplecontrols", web_simplecontrols),
		link("In-Place Textbox", "/web/advancedcontrols1", web_advancedcontrols1),
		link("Google Charts", "/web/advancedcontrols2", web_advancedcontrols2),
		link("Step-by-Step Wizard", "/web/advancedcontrols3", web_advancedcontrols3),
		link("Validation", "/web/validation", web_validation),
		#p{},
		
		#h3 { text="Effects, Events, and Ajax" },
		link("Postbacks", "/web/postback", web_postback),
		link("AJAX Replacement", "/web/ajax", web_ajax),
		link("User Notices", "/web/notices", web_notices),
		#p{},
		
		#h3 { text="User Interface" },
		link("Effects", "/web/effects", web_effects),
		link("Drag and Drop", "/web/dragdrop", web_dragdrop),
		link("Sorting", "/web/sorting1", web_sorting1),
		link("Nested Sorting", "/web/sorting2", web_sorting2),
		#p{},
		
		#h3 { text="Data Binding" },
		link("Simple (List-Based) Binding", "/web/binding1", web_binding1),
		link("Record-Based Binding", "/web/binding2", web_binding2),
		link("Key/Value Pair Binding", "/web/binding3", web_binding3),
		link("Binding With a Transform Function", "/web/binding4", web_binding4),
		#p{},
		
		#h3 { text="Templates" },
		link("No Template","/web/template_none", web_template_none),
		link("Simple Template", "/web/template_simple", web_template_simple),
		#p{},

		#h3 { text="Advanced Topics" },
		link("Continuations", "/web/continuations", web_continuations),
		#p{}
	]}},
	wf:render(Body).
	
link(Text, Url, Module) ->
	[
		#link { text=Text, url=Url },
		"&nbsp;", 
		#span { style="font-size: 60%;", text="(" },
		#link { style="font-size: 60%;", text="code", url=wf:f("/web/viewsource?module=~s", [Module])},
		#span { style="font-size: 60%;", text=")" },
		#br{}
	].
	
event(go) ->
	wf:flash("Hello there"),
	wf:update(test, "This is a test.");
	
event(_) -> ok.