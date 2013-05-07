
prolog_name('SWI Prolog').
prolog_version(X) :-
	current_prolog_flag(version, V),
	number_atom(V, X).
prolog_date(X) :-
	current_prolog_flag(compiled_at, X).
prolog_copyright('').

callable(X) :-
	atom(X), !.

callable(X) :-
	compound(X).



/* g_vars */

:-	dynamic(gvar / 2).

g_assign(Var, Value) :-
	(   retract(gvar(Var, _))
	;   true
	), !,
	asserta(gvar(Var, Value)).

g_read(Var, Value) :-
	(   gvar(Var, Value1)
	;   Value1 = 0
	), !,
	Value = Value1.



argument_list(LArgs) :-
	unix(argv([_|L])),
	delete_flags(L, LArgs), !.


delete_flags([], []).

delete_flags(['-x', _|L], L1) :-
	delete_flags(L, L1).

delete_flags(['-t', _|L], L1) :-
	delete_flags(L, L1).

delete_flags(['-g', _|L], L1) :-
	delete_flags(L, L1).

delete_flags([--|L], L).



go_other :-
	argument_list(L),
	go_other1(L).


go_other1([]) :-
	!.

go_other1(L) :-
	pl2wam(L),
	halt.
