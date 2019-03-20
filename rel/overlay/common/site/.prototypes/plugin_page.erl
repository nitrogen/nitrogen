%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module ([[[NAME]]]).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Testing plugin [[[NAME]]]!".

body() -> 
	wf:wire(#sample_action{}),
	[
		"You should see some more content and an alert: <b>",
		#sample_element{}, "</b>"
	].
