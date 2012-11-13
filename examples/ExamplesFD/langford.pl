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


langford(N, L) :-
	(   N mod 4 =:= 0
	;   N mod 4 =:= 3
	), !,
	length(U, N),
	length(V, N),
	append(U, V, UV),
	N2 is N * 2,
	fd_set_vector_max(N2),
	fd_domain(UV, 1, N2),
	fd_all_different(UV),
	set_cstr(U, V, 1),
	symetric(N, N2, UV),
%	fd_labeling(U, [variable_method(random), value_method(random)]), % sometimes much better
	fd_labeling(U, [variable_method(ff), value_method(max)]),
	decode(1, N, UV, L).


set_cstr([], [], _).

set_cstr([X|U], [Y|V], I) :-
	I1 is I + 1,
	Y - X #= I1,         % also avoid some symetries since enforces X < Y
	set_cstr(U, V, I1).


symetric(N, N2, UV) :-
	fd_element_var(I1, UV, 1),
	fd_element_var(I2, UV, N2),
	I1 #=< I2 - N.



decode(I, N, _, []) :-
	I > N * 2, !.

decode(I, N, UV, [Z1|L]) :-
	I1 is I + 1,
	nth(Z, UV, I),
	(   Z > N ->
	    Z1 is Z - N
	;   Z1 = Z
	),
	decode(I1, N, UV, L).



:-	initialization(q).
