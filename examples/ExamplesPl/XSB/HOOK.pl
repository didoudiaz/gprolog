% hook file for XSB Prolog

% Count is passed using command line argument -e 'assertz(count(Count)).'
get_count(Count) :-
	clause(count(Count),_).

get_cpu_time(T) :-
	cputime(X),
	T is floor(X*1000).

% no initialization, script executes q/0 after consult

