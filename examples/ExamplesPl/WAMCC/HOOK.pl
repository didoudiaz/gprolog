% hook file for Wamcc

% Count is passed as the first argument
get_count(Count) :-
	unix(argv(L)),
	L = [ACount|_],
	name(ACount, LCodes),
	name(Count, LCodes).

get_cpu_time(T) :-
	statistics(runtime, [T, _]).

:- main.
:- q, halt.

