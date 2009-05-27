/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : eq10.pl                                                */
/* Title          : linear equations                                       */
/* Original Source: Thomson LCR                                            */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : September 1992                                         */
/*                                                                         */
/* A system involving 7 variables and 10 equations                         */
/*                                                                         */
/* Solution:                                                               */
/*  [X1,X2,X3,X4,X5,X6,X7]                                                 */
/*  [ 6, 0, 8, 4, 9, 3, 9]                                                 */
/*-------------------------------------------------------------------------*/

q :-
	get_fd_labeling(Lab),
	statistics(runtime, _),
	eq10(LD, Lab),
	statistics(runtime, [_, Y]),
	write(LD),
	nl,
	write('time : '),
	write(Y),
	nl.




eq10(LD, Lab) :-
	LD = [X1, X2, X3, X4, X5, X6, X7],
	fd_domain(LD, 0, 10),
	0 + 98527 * X1 + 34588 * X2 + 5872 * X3 + 59422 * X5 + 65159 * X7 #= 1547604 + 30704 * X4 + 29649 * X6,
	0 + 98957 * X2 + 83634 * X3 + 69966 * X4 + 62038 * X5 + 37164 * X6 + 85413 * X7 #= 1823553 + 93989 * X1,
	900032 + 10949 * X1 + 77761 * X2 + 67052 * X5 #= 0 + 80197 * X3 + 61944 * X4 + 92964 * X6 + 44550 * X7,
	0 + 73947 * X1 + 84391 * X3 + 81310 * X5 #= 1164380 + 96253 * X2 + 44247 * X4 + 70582 * X6 + 33054 * X7,
	0 + 13057 * X3 + 42253 * X4 + 77527 * X5 + 96552 * X7 #= 1185471 + 60152 * X1 + 21103 * X2 + 97932 * X6,
	1394152 + 66920 * X1 + 55679 * X4 #= 0 + 64234 * X2 + 65337 * X3 + 45581 * X5 + 67707 * X6 + 98038 * X7,
	0 + 68550 * X1 + 27886 * X2 + 31716 * X3 + 73597 * X4 + 38835 * X7 #= 279091 + 88963 * X5 + 76391 * X6,
	0 + 76132 * X2 + 71860 * X3 + 22770 * X4 + 68211 * X5 + 78587 * X6 #= 480923 + 48224 * X1 + 82817 * X7,
	519878 + 94198 * X2 + 87234 * X3 + 37498 * X4 #= 0 + 71583 * X1 + 25728 * X5 + 25495 * X6 + 70023 * X7,
	361921 + 78693 * X1 + 38592 * X5 + 38478 * X6 #= 0 + 94129 * X2 + 43188 * X3 + 82528 * X4 + 69025 * X7,
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
