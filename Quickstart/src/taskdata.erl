-module (taskdata).
-compile(export_all).

get_tasks() -> 
	case wf:session(tasks) of
			undefined -> [];
			Other -> Other
	end.

save_tasks(Tasks) ->
	wf:session(tasks, Tasks).

replace(Old, New, [Old|T]) -> [New|T];
replace(Old, New, [H|T]) -> [H|replace(Old, New, T)];
replace(_, _, []) -> [].