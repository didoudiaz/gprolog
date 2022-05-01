/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : five.pl                                                */
/* Title          : five house puzzle                                      */
/* Original Source: P. Van Hentenryck's book                               */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : September 1992                                         */
/*                                                                         */
/* A logic puzzle                                                          */
/*                                                                         */
/* Solution:                                                               */
/*  [N1,N2,N3,N4,N5,     [3,4,5,2,1,                                       */
/*   C1,C2,C3,C4,C5,      5,3,1,2,4,                                       */
/*   P1,P2,P3,P4,P5,      5,1,4,2,3,                                       */
/*   A1,A2,A3,A4,A5,      4,5,1,3,2,                                       */
/*   D1,D2,D3,D4,D5]      4,1,2,5,3]                                       */
/*-------------------------------------------------------------------------*/

q :-
	get_fd_labeling(Lab),
	statistics(runtime, _),
	five_house(L, Lab),
	statistics(runtime, [_, Y]),
	write(L),
	nl,
	write('time : '),
	write(Y),
	nl.




five_house(L, Lab) :-
	fd_set_vector_max(5),
	L = [N1, N2, N3, N4, N5, C1, C2, C3, C4, C5, P1, P2, P3, P4, P5, A1, A2, A3, A4, A5, D1, D2, D3, D4, D5],
	fd_domain(L, 1, 5),
	N5 #= 1,
	D5 #= 3,
	fd_all_different([C1, C2, C3, C4, C5]),
	fd_all_different([P1, P2, P3, P4, P5]),
	fd_all_different([N1, N2, N3, N4, N5]),
	fd_all_different([A1, A2, A3, A4, A5]),
	fd_all_different([D1, D2, D3, D4, D5]),
	N1 #= C2,
	N2 #= A1,
	N3 #= P1,
	N4 #= D3,
	P3 #= D1,
	C1 #= D4,
	P5 #= A4,
	P2 #= C3,
	C1 #= C5 + 1,
	plus_or_minus(A3, P4, 1),
	plus_or_minus(A5, P2, 1),
	plus_or_minus(N5, C4, 1),
%           lab(Lab,L).    % faster than lab(Lab,[C1,...,D5])
	lab(Lab, [C1, C2, C3, C4, C5, P1, P2, P3, P4, P5, N1, N2, N3, N4, N5, A1, A2, A3, A4, A5, D1, D2, D3, D4, D5]).



	% partial lookahead

plus_or_minus(X, Y, C) :-
	X #= Y + C.

plus_or_minus(X, Y, C) :-
	X + C #= Y.




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
