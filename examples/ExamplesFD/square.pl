/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : square.pl                                              */
/* Title          : perfect square                                         */
/* Original Source: Pascal Van Hentenryck ([VHSD93])                       */
/* Adapted by     : Gregory Sidebottom (Nicolog) and Daniel Diaz (clp(FD)) */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : June 1994                                              */
/*                                                                         */
/* This program solves the perfect square packing problem (SPP): find a way*/
/* to pack all the given squares (i.e. known sizes) into a master rectangle*/
/* so that none overlap and there is no wasted space.                      */
/* There are 4 instances of the problem (P=3 corresponds to [VSHD93]).     */
/*                                                                         */
/* Solution:                                                               */
/* 1 x([ 0, 0,18,22,23,15,15,18,22])  2 x([ 0,41,42, 0,22,25,25,36,36,22]) */
/*   y([ 0,18, 0,14,24,25,18,14,24])    y([ 0,23, 0,25,28, 0,17,17,23,25]) */
/*   s([18,15,14,10, 9, 8, 7, 4, 1])    s([25,24,23,22,19,17,11, 6, 5, 3]) */
/*                                                                         */
/* 3 x([ 0,70,75, 0,79,50 ,0,50,46,27,52,35,59,35,35,50,27,52,46,75,50])   */
/*   y([ 0,70,33,50, 0, 0,85,29,88,93,70,65,54,50,82,54,85,63,82,29,63])   */
/*   s([50,42,37,35,33,29,27,25,24,19,18,17,16,15,11, 9, 8, 7, 6, 4, 2])   */
/*                                                                         */
/* 4 x([  0,111,  0, 56, 81,132, 72,  0,140,142,111,81,111, 38, 38, 56, 58,*/
/*       63,132, 58, 59, 56,140, 58])                                      */
/*   y([  0,111, 81, 81,  0,  0,136,137, 43, 78, 80,51, 51,155,137,136,161,*/
/*      152, 43,156,152,152, 78,155])                                      */
/*   s([ 81, 64, 56, 55, 51, 43, 39, 38, 35, 33, 31,30, 29, 20, 18, 16, 14,*/
/*        9,  8,  5,  4,  3,  2,  1])                                      */
/*-------------------------------------------------------------------------*/


q :-
	write('N ?'),
	read_integer(N),
	statistics(runtime, _),
	square(N, Xs, Ys, Ss),
	statistics(runtime, [_, Y]),
	write(x(Xs)),
	nl,
	write(y(Ys)),
	nl,
	write(s(Ss)),
	nl,
	write('time : '),
	write(Y),
	nl.




problem(1, 32, 33, [18, 15, 14, 10, 9, 8, 7, 4, 1]).

problem(2, 65, 47, [25, 24, 23, 22, 19, 17, 11, 6, 5, 3]).

problem(3, 112, 112, [50, 42, 37, 35, 33, 29, 27, 25, 24, 19, 18, 17, 16, 15, 11, 9, 8, 7, 6, 4, 2]).

problem(4, 175, 175, [81, 64, 56, 55, 51, 43, 39, 38, 35, 33, 31, 30, 29, 20, 18, 16, 14, 9, 8, 5, 4, 3, 2, 1]).

problem(5, 479, 479, [175, 174, 164, 160, 155, 150, 140, 130, 86, 77, 68, 60, 52, 44, 43, 35, 29, 28, 26, 24, 23, 17, 6, 5]).

problem(6, 655, 655, [288, 246, 216, 215, 194, 193, 173, 152, 151, 86, 84, 83, 65, 57, 54, 53, 51, 40, 31, 26, 25, 21, 15, 14, 10]).


% adaptive search examples

problem(-1, 112, 112, [50, 42, 37, 35, 33, 29, 27, 25, 24, 19, 18, 17, 16, 15, 11, 9, 8, 7, 6, 4, 2]).

problem(-4, 479, 479, [175, 174, 164, 160, 155, 150, 140, 130, 86, 77, 68, 60, 52, 44, 43, 35, 29, 28, 26, 24, 23, 17, 6, 5]).

problem(-5, 524, 524, [220, 164, 163, 159, 145, 141, 135, 132, 125, 101, 98, 90, 87, 62, 61, 55, 54, 39, 37, 35, 33, 21, 20, 12, 9]).




square(P, Xs, Ys, Ss) :-
	gen(P, Xs, Ys, Ss, SX, SY),
	(   SX >= SY ->
	    MaxS = SX
	;   MaxS = SY
	),
	fd_set_vector_max(MaxS),
	no_overlap(Xs, Ys, Ss),
	cap(Xs, Ss, SX, SY),
	cap(Ys, Ss, SY, SX),
	label(Xs),
	label(Ys).




gen(P, Xs, Ys, Ss, SX, SY) :-
	problem(P, SX, SY, Ss),
	gen_coords(Ss, Xs, Ys, SX, SY).




gen_coords([], [], [], _, _).

gen_coords([S|Ss], [X|Xs], [Y|Ys], SX, SY) :-
	X #=< SX - S,
	Y #=< SY - S,
	gen_coords(Ss, Xs, Ys, SX, SY).





no_overlap([], [], []).

no_overlap([X|Xs], [Y|Ys], [S|Ss]) :-
	no_overlap1(Xs, Ys, Ss, X, Y, S),
	no_overlap(Xs, Ys, Ss).




no_overlap1([], [], [], _, _, _).

no_overlap1([X2|Xs], [Y2|Ys], [S2|Ss], X1, Y1, S1) :-
	X1 + S1 #=< X2 #\/ X1 #>= X2 + S2 #\/ Y1 + S1 #=< Y2 #\/ Y1 #>= Y2 + S2,
	no_overlap1(Xs, Ys, Ss, X1, Y1, S1).





cap(Xs, Ss, SX, SY) :-
	cap1(0, SX, SY, Xs, Ss).


cap1(P, SX, SY, Xs, Ss) :-
	(   P < SX ->
	    sum_of_squares_with(Xs, Ss, P, SY),
	    P1 is P + 1,
	    cap1(P1, SX, SY, Xs, Ss)
	;   true
	).




sum_of_squares_with([], [], _, 0).

sum_of_squares_with([X|Xs], [S|Ss], P, Sum) :-
	point_used_by_square_iff_b(P, X, S, B),
	Sum #= S * B + Sum1,
	sum_of_squares_with(Xs, Ss, P, Sum1).




	% X<=P<X+S <=> B     P and S are ground

point_used_by_square_iff_b(P, X, S, B) :-
	B #<=> X #=< P #/\ P #< X + S.



label([]).

label([X|Xs]) :-
	list_min([X|Xs], Min),
	select_square([X|Xs], Min, Rest),
	label(Rest).




list_min([X|Xs], Min) :-
	fd_min(X, Min1),
	list_min1(Xs, Min1, Min).




list_min1([], M, M).

list_min1([X|Xs], M1, M) :-
	fd_min(X, M2),
	(   M1 =< M2 ->
	    M3 = M1
	;   M3 = M2
	),
	list_min1(Xs, M3, M).




select_square([X|Xs], X, Xs).

select_square([X|Xs], Min, [X|Rest]) :-
	X #> Min,
	select_square(Xs, Min, Rest).




:-	initialization(q).
