:- use_module(library(lists)).

% hook file for YAP Prolog

% Count is passed on the command line as -- Count
get_count(Count) :-
	unix(argv(L)),
	L = [ACount|_],
	atom_codes(ACount, LCodes),
	number_codes(Count, LCodes).

get_cpu_time(T) :-
	statistics(runtime, [T, _]).

:- initialization(q).
