/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : magic.pl                                               */
/* Title          : magic series                                           */
/* Original Source: W.J. Older and F. Benhamou - Programming in CLP(BNR)   */
/*                  (in Position Papers of PPCP'93)                        */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : May 1993                                               */
/*                                                                         */
/* A magic serie is a sequence x0, x1, ..., xN-1 such that each xi is the  */
/* number of occurences of i in the serie.                                 */
/*           N-1                                                           */
/*  ie  xi = Sum (xj=i)  where (xj=i) is 1 if x=y and 0 if x<>y            */
/*           i=0                                                           */
/*                                                                         */
/* two redundant constraints are used:                                     */
/*           N-1                     N-1                                   */
/*           Sum i = N          and  Sum i*xi = N                          */
/*           i=0                     i=0                                   */
/*                                                                         */
/* Note: in the Pascal's original version the length of a magic serie is   */
/* N+1 (x0, x1, ..., XN) instead of N (x0, x1, ..., xN-1). Finding such a  */
/* serie (for N) only corresponds to find a serie for N+1 in this version. */
/* Also the original version only used one redundant constraint.           */
/*                                                                         */
/* Solution:                                                               */
/* N=1,2,3 and 6 none                                                      */
/* N=4  [1,2,1,0] and [2,0,2,0]                                            */
/* N=5  [2,1,2,0,0]                                                        */
/* N=7  [3,2,1,1,0,0,0]   (for N>=7  [N-4,2,1,<N-7 0's>,1,0,0,0])          */
/*-------------------------------------------------------------------------*/

q :-
	get_fd_labeling(Lab),
	write('N ?'),
	read_integer(N),
	statistics(runtime, _),
	magic(N, L, Lab),
	statistics(runtime, [_, Y]),
	write(L),
	nl,
	write('time : '),
	write(Y),
	nl.




magic(N, L, Lab) :-
	fd_set_vector_max(N),
	length(L, N),
	fd_domain(L, 0, N),
	constraints(L, L, 0, N, N),
	lab(Lab, L).




constraints([], _, _, 0, 0).

constraints([X|Xs], L, I, S, S2) :-
	sum(L, I, X),
	I1 is I + 1,
	S1 + X #= S,                                 % redundant constraint 1
	(   I = 0 ->
	    S3 = S2
	;   I * X + S3 #= S2
	),                                           % redundant constraint 2
	constraints(Xs, L, I1, S1, S3).




sum([], _, 0).

sum([X|Xs], I, S) :-
	sum(Xs, I, S1),
	X #= I #<=> B,
	S #= B + S1.




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
