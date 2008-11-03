-module (action_effect_pulsate).
-include ("wf.inc").
-compile(export_all).

render_action(_TriggerPath, TargetPath, Record) -> 
	Pulses = Record#effect_pulsate.pulses,
	Duration = Record#effect_pulsate.duration,
	[
		wf:me_var(), 
		wf:f("new Effect.Pulsate(obj('~s'), { pulses: ~p, duration: ~f });", [wf:to_ident(TargetPath), Pulses, Duration])	
	].