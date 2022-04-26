/*
 * Try to remove all context specific code to obtain a .wam file which can 
 * compared to the code produced without context support (using diff).
 * The obtained code is NOT executable 
 * Operation performed:
 * - replace cxt_call (cxt_execute) by call (execute)
 * - remove a get_variable(V, 255) storing V in a global var
 *   if V is x(N) (y(N)) then all an instruction using x(N1) (y(N1))
 *   is removed if N1 = N
 *   is replaced by x(N1-1) (y(N1-1)) if N1 > N
 *   if V is y(_) the previous allocate(N) becomes allocate(N-1)
 */

cxt_for_diff_emit(WamCode, WamCode1) :-
	cxt_for_diff_init_clause,
	cxt_for_diff(WamCode, WamCode1), !.
	

	

cxt_for_diff_init_clause :-
	g_assign(cxt_K_xy, '$cxt_null'),
	g_assign(cxt_K_no, -1).




cxt_for_diff([], []).

cxt_for_diff([L1|L2], [DL1|DL2]) :-
	cxt_for_diff(L1, DL1),
	cxt_for_diff(L2, DL2).

cxt_for_diff([cxt_call(PN,_)|L], [call(PN)|DL]) :-
	cxt_for_diff(L, DL).

cxt_for_diff([cxt_execute(PN,_)|L], [execute(PN)|DL]) :-
	cxt_for_diff(L, DL).

cxt_for_diff([get_variable(V, 255)|L], DL) :-
	functor(V, XY, _),
	arg(1, V, No),
	g_assign(cxt_K_xy, XY),
	g_assign(cxt_K_no, No),
	(   XY = y ->
	    g_read(cxt_allocate, A),
	    A = allocate(N),
	    N1 is N - 1,
	    setarg(1, A, N1),
	    g_assign(cxt_allocate, '$cxt_null')
	;
	    true
	),
	cxt_for_diff(L, DL).

cxt_for_diff([A|L], [A|DL]) :-
	A = allocate(_),
	g_link(cxt_allocate, A),
	cxt_for_diff(L, DL).

cxt_for_diff([Lab|L], [Lab|DL]) :-
	Lab = label(_),
	cxt_for_diff_init_clause,
	cxt_for_diff(L, DL).

cxt_for_diff([I|L], R) :-
	(   cxt_fix_vars(I, I1) ->
	    R = [I1|DL]
	;   R = DL
	),
	cxt_for_diff(L, DL).

cxt_for_diff([I|L], [I|DL]) :-
	cxt_for_diff(L, DL).




cxt_fix_vars(I, I) :-
	atomic(I), !.

cxt_fix_vars(I, I1) :-
	I =.. [XY, No1],
	g_read(cxt_K_xy, XY),
	g_read(cxt_K_no, No2),
	!,
	(   No1 = No2 ->
	    fail
	;   No1 > No2 ->
	    NewNo is No1 - 1
	;   NewNo = No1
	),
	I1 =.. [XY, NewNo].

cxt_fix_vars(x(X), x(X)) :- !.

cxt_fix_vars(y(Y), y(Y)) :- !.

cxt_fix_vars(I, I1) :-
	I =.. L,
	cxt_fix_vars_lst(L, L1),
	I1 =.. L1.


cxt_fix_vars_lst([], []).

cxt_fix_vars_lst([X|L], [X1|L1]) :-
	cxt_fix_vars(X, X1),
	cxt_fix_vars_lst(L, L1).

