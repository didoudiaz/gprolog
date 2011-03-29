/*-------------------------------------------------------------------------*/
/* Benchmark (Boolean)                                                     */
/*                                                                         */
/* Name           : bsend.pl                                               */
/* Title          : crypt-arithmetic                                       */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : January 1993                                           */
/*                                                                         */
/* Solve the operation:                                                    */
/*                                                                         */
/*      S E N D                                                            */
/*  +   M O R E                                                            */
/*  -----------                                                            */
/*  = M O N E Y                                                            */
/*                                                                         */
/* (resolution by column)                                                  */
/* The digit of each letter is coded in binary on 4 bits (dcb). The order  */
/* for labeling is very relevant for efficiency.                           */
/*                                                                         */
/* Solution:                                                               */
/*  [S,E,N,D,M,O,R,Y]                                                      */
/*  [[1,0,0,1],[0,1,0,1],[0,1,1,0],[0,1,1,1],[0,0,0,1],[0,0,0,0],[1,0,0,0],*/
/*   [0,0,1,0]]                                                            */
/* ie:                                                                     */
/*  [9,5,6,7,1,0,8,2]                                                      */
/*-------------------------------------------------------------------------*/

q :-
	statistics(runtime, _),
	(   bsend(A),
	    write(A),
	    nl                                                             %,
%        fail
	;   write('No more solutions'),
	    nl
	),
	statistics(runtime, [_, Y]),
	write('time : '),
	write(Y),
	nl.




bsend(A) :-
	A = [S, E, N, D, M, O, R, Y],
	dcb_digit(S),
	dcb_digit(E),
	dcb_digit(N),
	dcb_digit(D),
	dcb_digit(M),
	dcb_digit(O),
	dcb_digit(R),
	dcb_digit(Y),
	diff0(S),
	diff0(M),
	all_dcb_digit_diff(A),
	LC = [C1, C2, C3, C4],
	Z = [0, 0, 0, 0],
	dcb_add(0, D, E, Y, C1),
	dcb_add(C1, N, R, E, C2),
	dcb_add(C2, E, O, N, C3),
	dcb_add(C3, S, M, O, C4),
	dcb_add(C4, Z, Z, M, 0), !,
	array_labeling(A),
	fd_labeling(LC).



dcb_digit(D) :-
	D = [B3, B2, B1, _],
	B3 #==> #\ B2 #/\ #\ B1.




diff0([B3, B2, B1, B0]) :-
	B3 #\/ B2 #\/ B1 #\/ B0.




all_dcb_digit_diff([]).

all_dcb_digit_diff([X|L]) :-
	diff_of(L, X),
	all_dcb_digit_diff(L).




diff_of([], _).

diff_of([Y|L], X) :-
	dcb_digit_diff(X, Y),
	diff_of(L, X).



dcb_digit_diff([X3, X2, X1, X0], [Y3, Y2, Y1, Y0]) :-
	#\  ((X3 #<=> Y3) #/\ (X2 #<=> Y2) #/\ (X1 #<=> Y1) #/\ (X0 #<=> Y0)).




dcb_add(CI, [X3, X2, X1, X0], [Y3, Y2, Y1, Y0], [Z3, Z2, Z1, Z0], CO) :-
	full_add(CI, X0, Y0, Z0, C1),
	full_add(C1, X1, Y1, I1, C2),
	full_add(C2, X2, Y2, I2, C3),
	full_add(C3, X3, Y3, I3, C4),
	I2 #\/ I1 #<=> I12,
	I3 #/\ I12 #<=> I123,
	C4 #\/ I123 #<=> Hex,
	half_add(I1, Hex, Z1, D2),
	full_add(D2, I2, Hex, Z2, D3),
	half_add(D3, I3, Z3, D4),
	C4 #\/ D4 #<=> CO.




full_add(CI, X, Y, Z, CO) :-
	half_add(X, Y, Z1, C1),
	half_add(CI, Z1, Z, C2),
	C1 #\/ C2 #<=> CO.




half_add(X, Y, Z, CO) :-
	X #/\ Y #<=> CO,
	X ## Y #<=> Z.




:-	include(array).

% interface with for_each_... procedures

array_prog(_, _).




:-	initialization(q).
