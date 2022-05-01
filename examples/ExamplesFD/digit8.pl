/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : digit8.pl                                              */
/* Title          : particular 8 digit number                              */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : October 1993                                           */
/*                                                                         */
/* Find the 8 digit number N such that:                                    */
/*                                                                         */
/*    - N is a square                                                      */
/*    - if we put a 1 in front of the decimal notation of N then it is     */
/*      still a square                                                     */
/*                                                                         */
/* Solution:                                                               */
/*  [N,X,M,Y]                                                              */
/*  [23765625,4875,123765625,11125]                                        */
/*  [56250000,7500,156250000,12500]                                        */
/*-------------------------------------------------------------------------*/

q :-
	get_fd_labeling(Lab),
	statistics(runtime, _),
	(   digit8(L, Lab),
	    write(L),
	    nl,
	    fail
	;   write('No more solutions'),
	    nl
	),
	statistics(runtime, [_, Y]),
	write('time : '),
	write(Y),
	nl.




digit8(L, Lab) :-
	L = [N, X, M, Y],
	N #>= 10000000,
	N #=< 99999999,
	X ** 2 #= N,
	100000000 + N #= M,
	Y ** 2 #= M,
	lab(Lab, L).




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
