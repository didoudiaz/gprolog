% hook file for SWI Prolog
% Count is passed on the command line as the last argument (1st is 'pl')

get_count(Count) :-
	unix(argv(L)),
	get_last(L, ACount),
	atom_codes(ACount, LCodes),
	number_codes(Count, LCodes).

get_last([Count], Count):-
	!,
	sub_atom(Count, 0, 1, _, X),
	X @>= '0',
	X @=< '9'.

get_last([_|L], Count):-
	get_last(L, Count).


get_cpu_time(T) :-
	statistics(cputime, X),
	T is X*1000.

:- initialization(q).
