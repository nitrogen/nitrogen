-module (web_reference).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', reference},
	{'Item', overview}
]}.

title() -> "Nitrogen Reference Overview".
headline() -> "Nitrogen Reference Overview".

body() -> [
	"
		<p>
		<h2><a href='reference/elements'>Elements</a></h2>
		Elements are the building blocks of a Nitrogen page. They let you specify
		a page template, divs, spans, textboxes, and buttons using a simple syntax based on
		Erlang records.
		<p>
		<h2><a href='reference/actions'>Actions</a></h2>
		Actions let you make a page dynamic. Use actions to wire up click or mouseover
		events, apply JQuery effects, show or hide elements, and display alerts.
		<p>
		<h2><a href='reference/validators'>Validators</a></h2>
		Validators help you check that the data a user entered is valid. Check for
		required fields, integer values, emails, and others, or add your own
		custom validators.
	"
].
