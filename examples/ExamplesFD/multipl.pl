/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : multipl.pl                                             */
/* Title          : unknown multiplication                                 */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : June 1995                                              */
/*                                                                         */
/* Find the value of each digit verifying the following multiplication and */
/* such that each digit (0,1,...,9) appears excatly twice:                 */
/*                                                                         */
/*                   X1  X2  X3                                            */
/*                 * X4  X5  X6                                            */
/*                  -----------                                            */
/*                   X7  X8  X9                                            */
/*        +      X10 X11 X12                                               */
/*        + X13 X14 X15                                                    */
/*        = -------------------                                            */
/*          X16 X17 X18 X19 X20                                            */
/*                                                                         */
/* Solution:                                                               */
/* [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20]*/
/* [ 1, 7, 9, 2, 2, 4, 7, 1, 6,  3,  5,  8,  3,  5,  8,  4,  0,  0,  9,  6]*/
/*-------------------------------------------------------------------------*/

q :-
	get_fd_labeling(Lab),
	statistics(runtime, _),
	mult(Lab, LD),
	statistics(runtime, [_, Y]),
	write(LD),
	nl,
	write('time : '),
	write(Y),
	nl.


mult(Lab, LD) :-
	fd_set_vector_max(9),
	LD = [X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20],
	fd_domain(LD, 0, 9),
	fd_atmost(2, LD, 0),
	fd_atmost(2, LD, 1),
	fd_atmost(2, LD, 2),
	fd_atmost(2, LD, 3),
	fd_atmost(2, LD, 4),
	fd_atmost(2, LD, 5),
	fd_atmost(2, LD, 6),
	fd_atmost(2, LD, 7),
	fd_atmost(2, LD, 8),
	fd_atmost(2, LD, 9),
/* This is much slower...
        fd_exactly(2,LD,0),
        fd_exactly(2,LD,1),
        fd_exactly(2,LD,2),
        fd_exactly(2,LD,3),
        fd_exactly(2,LD,4),
        fd_exactly(2,LD,5),
        fd_exactly(2,LD,6),
        fd_exactly(2,LD,7),
        fd_exactly(2,LD,8),
        fd_exactly(2,LD,9),
*/
	Y #= 100 * X1 + 10 * X2 + X3,
	Z1 #= 100 * X7 + 10 * X8 + X9,
	Z2 #= 100 * X10 + 10 * X11 + X12,
	Z3 #= 100 * X13 + 10 * X14 + X15,
	X6 * Y #= Z1,
	X5 * Y #= Z2,
	X4 * Y #= Z3,
	100 * X7 + 10 * X8 + X9 + 1000 * X10 + 100 * X11 + 10 * X12 + 10000 * X13 + 1000 * X14 + 100 * X15 #= 10000 * X16 + 1000 * X17 + 100 * X18 + 10 * X19 + X20,
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
