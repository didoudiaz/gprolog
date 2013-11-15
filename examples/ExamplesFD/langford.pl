/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : langford.pl                                            */
/* Title          : Langford's problem                                     */
/* Original Source: Daniel Diaz                                            */
/* Date           : February 2003                                          */
/*                                                                         */
/* The problem L(K,N) is to arrange K sets of numbers 1 to N, so that each */
/* appearance of the number M is M numbers on from the last. We here solve */
/* L(2,N). The problem admits a solution if N is of the form 4k or 4k-1.   */
/*                                                                         */
/* Solution:                                                               */
/* N=4  [2,3,4,2,1,3,1,4]                                                  */
/* N=8  [1,5,1,6,4,7,8,5,3,4,6,2,3,7,2,8]                                  */
/* N=11 [5,1,2,1,9,2,5,8,10,11,4,6,7,3,9,4,8,3,6,10,7,11]                  */
/*-------------------------------------------------------------------------*/

q :-
	write('N ?'),
	read_integer(N),
	statistics(runtime, _),
	langford(N, L),
	statistics(runtime, [_, Y]),
	write(L),
	nl,
	write('time : '),
	write(Y),
	nl.


/*
 * Find an assignment of [X1, X2, ..., Xn] (Xi is the position of the first occurrence of i).
 * For each Xi the constraints are: 
 *
 *    Xi in 1..(N+N-1-i)
 *    if each pair Xi, Xj (i < j) the following holds:
 *    Xj != Xi
 *    Xj != Xi + i + 1
 *    Xj != Xi - j - 1
 *    Xj != Xi + i - j
 *
 * This can be achieved using #=# in set_cstr (but slower at the end)
 *
 * Here we keep the list of positions Ui (resp. Vi) is the position of the first (resp. second) occurrence of i
 * It is possible to only keep one list (i.e. Ui) but seems slower (see file LANGFORD.pl)
 */


langford(N, LD) :-
	(   N mod 4 =:= 0
	;   N mod 4 =:= 3
	), !,
	length(U, N),
	length(V, N),
	append(U, V, L),
	N2 is N * 2,
	fd_set_vector_max(N2),
	fd_domain(L, 1, N2),
	fd_all_different(L),
	set_cstr(U, V, 1),
	symetric(N, N2, L),
%	fd_labeling(U, [variable_method(random), value_method(random)]), % sometimes much better
	fd_labeling(U, [variable_method(ff), value_method(max)]),
	decode(U, N2, LD).


/*
 * Find an assignment of [X1, X2, ..., Xn] (Xi is the position of the first occurrence of i).
 * For each Xi the constraints are: 
 *
 *    Xi in 1..(N+N-1-i)
 *    if each pair Xi, Xj (i < j) the following holds:
 *    Xi != Xj
 *    Xj != Xi + i + 1
 *    Xi != Xj + j + 1
 *    Xi + i + 1 != Xj + j + 1
 *
 * This can be achieved using #=# in set_cstr (but slower at the end)
 */


set_cstr([], [], _).

set_cstr([X|U], [Y|V], I) :-
	I1 is I + 1,
	Y #= X + I1,         % also avoid some symetries since enforces X < Y
%	Y #=# X + I1,         % better pruning but slower for big values
	set_cstr(U, V, I1).


symetric(N, N2, UV) :-
	fd_element_var(I1, UV, 1),
	fd_element_var(I2, UV, N2),
	I1 #=< I2 - N.




decode(L, N2, LD) :-
	length(LD, N2),
	decode1(L, 1, LD).


decode1([], _, _).

decode1([X|L], I, LD) :-
	nth(X, LD, I),
	Y is X + I + 1,
	nth(Y, LD, I),	
	I1 is I + 1,
	decode1(L, I1, LD).



:-	initialization(q).
