/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : send.pl                                                */
/* Title          : crypt-arithmetic                                       */
/* Original Source: P. Van Hentenryck's book                               */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : September 1992                                         */
/*                                                                         */
/* Solve the operation:                                                    */
/*                                                                         */
/*      S E N D                                                            */
/*  +   M O R E                                                            */
/*  -----------                                                            */
/*  = M O N E Y                                                            */
/*                                                                         */
/* (resolution by line)                                                    */
/*                                                                         */
/* Solution:                                                               */
/*  [S,E,N,D,M,O,R,Y]                                                      */
/*  [9,5,6,7,1,0,8,2]                                                      */
/*-------------------------------------------------------------------------*/

q :-
	get_fd_labeling(Lab),
	statistics(runtime, _),
	send(LD, Lab),
	statistics(runtime, [_, Y]),
	write(LD),
	nl,
	write('time : '),
	write(Y),
	nl.




send(LD, Lab) :-
	LD = [S, E, N, D, M, O, R, Y],
	fd_all_different(LD),
	fd_domain(LD, 0, 9),
	fd_domain([S, M], 1, 9),
	1000 * S + 100 * E + 10 * N + D + 1000 * M + 100 * O + 10 * R + E #= 10000 * M + 1000 * O + 100 * N + 10 * E + Y,
	lab(Lab, LD).




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
