/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : queens.pl                                              */
/* Title          : N-queens problem                                       */
/* Original Source: P. Van Hentenryck's book                               */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : January 1993                                           */
/*                                                                         */
/* Put N queens on an NxN chessboard so that there is no couple of queens  */
/* threatening each other.                                                 */
/*                                                                         */
/* Solution:                                                               */
/* N=4  [2,4,1,3]                                                          */
/* N=8  [1,5,8,6,3,7,2,4]                                                  */
/* N=16 [1,3,5,2,13,9,14,12,15,6,16,7,4,11,8,10]                           */
/*-------------------------------------------------------------------------*/


q :-
	get_fd_labeling(Lab),
	write('N ?'),
	read_integer(N),
	statistics(runtime, _),
	queens(N, L, Lab),
	statistics(runtime, [_, Y]),
	write(L),
	nl,
	write('time : '),
	write(Y),
	nl.




queens(N, L, Lab) :-
	fd_set_vector_max(N),
	length(L, N),
	fd_domain(L, 1, N),
	safe(L),
	lab(Lab, L).




safe([]).

safe([X|L]) :-
	noattack(L, X, 1),
	safe(L).




noattack([], _, _).


noattack([Y|L], X, I):-
	I1 is I + 1,
	noattack(L, X, I1),
	diff(X, Y ,I).

/*
% slower version (term. rec) (original PVH's version)
noattack([Y|L], X, I) :-
	diff(X, Y, I),
	I1 is I + 1,
	noattack(L, X, I1).
*/


diff(X, Y, I) :-
	fd_tell(diff(X, Y, I)).

/*
diff(X, Y, I):-
	X #\= Y,
	X #\= Y + I,
	X+I #\= Y.
*/

lab(normal, L) :-
	fd_labeling(L).

lab(ff, L) :-
	fd_labelingff(L).




get_fd_labeling(Lab) :-
	argument_counter(C),
	get_labeling1(C, Lab).


get_labeling1(1, normal).

get_labeling1(2, Lab) :-
	argument_value(1, Lab).


:-	initialization(q).
