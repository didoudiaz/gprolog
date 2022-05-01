
prolog_name('YAP Prolog').
prolog_version('4.3.12').
prolog_date('2000').
prolog_copyright('').



g_assign(Var, Value) :-
	bb_put(Var, Value).

g_read(Var, Value) :-
	(   bb_get(Var, Value1)
	;   Value1 = 0
	), !,
	Value = Value1.




argument_list([]).




reverse([], []).

reverse([H|T], L) :-
	reverse1(T, L, [H]).


reverse1([], L, L).

reverse1([H|T], L, L1) :-
	reverse1(T, L, [H|L1]).




append([], L, L).

append([X|L1], L2, [X|L3]) :-
	append(L1, L2, L3).


% execution


go_other :-
	argument_list(L),
	go_other1(L).


go_other1([]) :-
	!.

go_other1(L) :-
	pl2wam(L),
	halt.


:-	initialization(go_other).
