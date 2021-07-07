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
 *    Xi != Xj
 *    Xj != Xi + i + 1
 *    Xi != Xj + j + 1
 *    Xi + i + 1 != Xj + j + 1
 */

langford(N, LD) :-
	(   N mod 4 =:= 0
	;   N mod 4 =:= 3
	), !,
	length(L, N),
	N2 is N * 2,
	fd_set_vector_max(512),
	set_cstr(L, L, 1, N2),
	fd_all_different(L),
	symetric(L, N),
%	fd_labeling(L, [variable_method(random), value_method(random)]), % sometimes much better
	fd_labeling(L, [variable_method(ff), value_method(max)]),
	decode(L, N2, LD).




set_cstr([], _, _, _).

set_cstr([X|U], L, I, N2) :-
	Max is N2 - 1 - I,
	fd_domain(X, 1, Max),
	I1 is I + 1,
	set_cstr1(U, I1, X, I1),
	set_cstr(U, L, I1, N2).



% TO DO: don't recompute X + I1 and Y + J1 (create several same variables)
set_cstr1([], _, _, _).

set_cstr1([Y|L], J, X, I1) :-	
	J1 is J + 1,
	% X #\= Y, % done by all_different
	Y #\= X + I1,		% I1 == I + 1, thus we state Y #\= X + I + 1
	X #\= Y + J1,		% J1 == J + 1, thus we state X #\= Y + J + 1
%	Y + J1 #\= X + I1,
	(   I1 > J1 ->
	    Diff is I1 - J1,
	    Y #\= X + Diff
	;
	    Diff is J1 - I1,
	    Y + Diff #\= X
	),
	set_cstr1(L, J1, X, I1).




symetric([X|_], N) :-
	X #< N.


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
