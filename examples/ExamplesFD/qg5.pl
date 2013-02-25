/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : qg5.pl                                                 */
/* Title          : Quasi-group problem QG5                                */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : July 1998, modified 2009                               */
/*                                                                         */
/* Find a semigroup table so that: ((xy)x)x=y under idempotency hypothesis.*/
/*                                                                         */
/* Solution:                                                               */
/* N = 5   [[1,5,4,2,3],[3,2,5,1,4],[2,4,3,5,1],[5,3,1,4,2],[4,1,2,3,5]]   */
/*                                                                         */
/* table (x.y is at col x, row y)                                          */
/*   1  5  4  2  3                                                         */
/*   3  2  5  1  4                                                         */
/*   2  4  3  5  1                                                         */
/*   5  3  1  4  2                                                         */
/*   4  1  2  3  5                                                         */
/*-------------------------------------------------------------------------*/


q :-
	write('N ?'),
	read_integer(N),
	statistics(runtime, _),
	qg5(N, A),
	statistics(runtime, [_, Y]),
	write(A),
	nl,
	write_array(A, '%3d', 0),
	write('time : '),
	write(Y),
	nl.


qg5(N, A) :-
	fd_set_vector_max(N),
	create_array(N, N, A),
	array_values(A, L),
	fd_domain(L, 1, N),
	for_each_line(A, alldiff),
	for_each_column(A, alldiff),
	last(A, Last),
	isomorphic_cstr(Last, 0),
	axioms_cstr(1, N, A),
	fd_labelingff(L).



array_prog(alldiff, L) :-
	fd_all_different(L).



isomorphic_cstr([], _).

isomorphic_cstr([X|L], K) :-
	X #>= K,
	K1 is K + 1,
	isomorphic_cstr(L, K1).




axioms_cstr(I, N, A) :-
	I =< N, !,
	nth(I, A, L),
	axioms_cstr1(1, N, I, L, A),
	I1 is I + 1,
	axioms_cstr(I1, N, A).

axioms_cstr(_, _, _).


axioms_cstr1(J, N, I, L, A) :-
	J =< N, !,
	array_elem(A, J, I, V1),
	(   I = J ->
	    V1 = I                                              % idempotency
	;   fd_element_var(V1, L, V2),
	    fd_element_var(V2, L, J)
	),
	J1 is J + 1,
	axioms_cstr1(J1, N, I, L, A).

axioms_cstr1(_, _, _, _, _).



:-	include(array).

:-	initialization(q).
