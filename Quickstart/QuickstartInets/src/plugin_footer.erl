-module (plugin_footer).
-include ("wf.inc").
-export ([render_in_template/1]).

render_in_template(_Rec) ->
	["
		<div class=col>
			<h3>About Nitrogen</h3>
			Nitrogen was created to give Erlang hackers the same caliber of web development tools that other 
			languages enjoy.
			<p>
			With Erlang+Nitrogen, you can rapidly develop infinitely scaleable, Ajax-rich web applications with
			a pure Erlang technology stack.
		</div>

		<div class=col>
			<h3>Production Tested</h3>
			Nitrogen powers the website of <a href='http://stitcho.com'>Stitcho.com</a>, a 
			web notification platform with support for both Windows and Mac.
		</div>

		<div class=col>
			<h3>Get Involved</h3>
			<ul>
				<li>Sign up on GitHub, watch the <a href='http://github.com/rklophaus/nitrogen'>Nitrogen repository</a>, and add to the wiki.
				<li>Post messages on the <a href='http://groups.google.com/group/nitrogenweb'>Nitrogen Google Group</a>.
				<li>Follow <a href='http://twitter.com/rklophaus'>rklophaus</a> on Twitter.
				<li>Nitrogen needs a new logo, <a href='nitrogenframework@gmail.com'>email</a> if you can help.
			</ul>
		</div>
		<p clear=both>
	"].
