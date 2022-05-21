% hook file for BinProlog

% Count is passed as a fact at the end of the source
get_count(Count) :-
	count(Count).

get_cpu_time(T) :-
	statistics(runtime,[_,T]).

% no initialization, script executes q/0 after consult

main :-
	q.
