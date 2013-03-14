/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : magsq.pl                                               */
/* Title          : Magic square problem                                   */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : July 1998                                              */
/*                                                                         */
/* Fill square NxN with integers 1,2...N*N so that each line, each column  */
/* and each diagonal has the same sum.                                     */
/* threatening each other.                                                 */
/*                                                                         */
/* Solution:                                                               */
/* N=3  [[4,9,2],[3,5,7],[8,1,6]]                                          */
/*-------------------------------------------------------------------------*/


q :-
	write('N ?'),
	read_integer(N),
	statistics(runtime, _),
	magsq(N, A),
	statistics(runtime, [_, Y]),
	write(A),
	nl,
	write_array(A, '%5d', 0),
	write('time : '),
	write(Y),
	nl.


magsq(N, A) :-
	create_array(N, N, A),
	N2 is N * N,
	fd_set_vector_max(N2),
	S is N * (N2 + 1) // 2,
	g_assign(s, S),
	g_assign(n, N),
	g_assign(n2, N2),
	array_values(A, Values),
	fd_all_different(Values),
	for_each_line(A, dom),
	for_each_line(A, sum),
	for_each_column(A, sum),
	for_each_big_diagonal(A, N, sum),
	array_elem(A, 1, 1, X11),
	array_elem(A, 1, N, X1N),
	array_elem(A, N, 1, XN1),
	array_elem(A, N, N, XNN),
	X11 #< X1N,	% 4 symmetry breaking constraints
	X11 #< XN1,
	X11 #< XNN,
	XN1 #> X1N,
	for_each_big_diagonal(A, N, lab),
%       for_each_line(A,lab).
	fd_labeling(Values, [variable_method(ff), value_method(max)]).
% in practice this random is better than max
%	fd_labeling(Values, [variable_method(ff), value_method(random)]).




array_prog(dom, L) :-
	g_read(n2, N2),
	fd_domain(L, 1, N2).

array_prog(sum, L) :-
	g_read(s, S),
	sum(L, S).

array_prog(lab, L) :-
	fd_labeling(L, [value_method(middle)]).


/*
reorder(L,L1):-
	g_read(n,N),
	fd_domain(X,1,N),
	findall(V,(fd_labeling(X,[value_method(middle)]),nth(X,L,V)),L1).
*/



sum([], 0).

sum([X|Xs], S) :-
	S #= X + S1,
	sum(Xs, S1).


:-	include(array).

:-	initialization(q).
