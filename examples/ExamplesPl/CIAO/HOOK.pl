% hook file for CIAO Prolog

:- use_module(library(prolog_sys), [statistics/2]).

% Count is passed as first argument
get_count(Count) :-
	current_prolog_flag(argv, L),
	L = [ACount|_],
	atom_codes(ACount, LCodes),
	number_codes(Count, LCodes).

get_cpu_time(T) :-
	statistics(runtime, [T, _]).

% main/0 needed by ciaoc
main.

:- initialization(q).
