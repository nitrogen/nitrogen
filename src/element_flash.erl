% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_flash).
-include ("wf.inc").
-compile(export_all).

render_in_template(_Record) ->
	wf:render(#flash { id=flash }).
	
update() ->
	wf:insert_bottom(flash, get_flashes()).

render(ControlID, _Record) -> 
	Terms = #panel { 
		class=flash_container,
		body=get_flashes()
	},
	element_panel:render(ControlID, Terms).
	
add_flash(Term) ->
	Flashes = case wf:session(flashes) of
		undefined -> [];
		X -> X
	end,
	wf:session(flashes, [Term|Flashes]),
	ok.

get_flashes() -> 
	% Create terms for an individual flash...
	F = fun(X) ->
		FlashID = wf:temp_id(),
		InnerPanel = #panel { class=flash, actions=#effect_blinddown { target=FlashID, duration=0.4 }, body=[
			#link { class=flash_close_button, text="Close", actions=#event { type=click, target=FlashID, actions=#effect_blindup {} } },
			#panel { class=flash_content, body=X }
		]},
		#panel { id=FlashID, style="display: none;", body=InnerPanel}
	end,
	
	% Get flashes, and clear session...
	Flashes = case wf:session(flashes) of 
		undefined -> [];
		Other -> Other
	end,	
	wf:session(flashes, []),
	
	% Return list of terms...
	[F(X) || X <- lists:reverse(Flashes)].