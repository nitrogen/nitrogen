% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_flash).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, flash).

render() -> 
	wf:state(has_flash, true),
	wf:render(#flash { }).
	
update() -> update(any).
update(element_flash) -> ok;
update(_Module) ->
	case wf:state(has_flash) == true andalso get(is_redirect) /= true of
		true -> wf:insert_bottom(flash, get_flashes());
	  _ -> ignore
	end.

render(_ControlID, _Record) -> 
	Terms = #panel { 
		id=flash,
		class=flash_container,
		body=get_flashes()
	},
	wf:state(has_flash, true),
	wf:render(Terms).
	
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
		InnerPanel = #panel { class=flash, actions=#show { target=FlashID, effect=blind, speed=400 }, body=[
			#link { class=flash_close_button, text="Close", actions=#event { type=click, target=FlashID, actions=#hide { effect=blind, speed=400 } } },
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