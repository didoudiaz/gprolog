% hook file for SICStus Prolog

% Count is passed on the command line as -a Count
get_count(Count) :-
	current_prolog_flag(argv, L),
	L = [ACount|_],
	atom_codes(ACount, LCodes),
	number_codes(Count, LCodes).

get_cpu_time(T) :-
	statistics(runtime, [T, _]).

:- initialization(q).
