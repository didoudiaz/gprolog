/*-------------------------------------------------------------------------*/
/* Benchmark (Boolean)                                                     */
/*                                                                         */
/* Name           : bdiag.pl                                               */
/* Title          : N adder diagnostic                                     */
/* Original Source: Greg Sidebottom - University of Vancouver Canada       */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : September 1993                                         */
/*                                                                         */
/* The circuit diagnosis problem is as follows:                            */
/*                                                                         */
/*Given:                                                                   */
/*  1. a description of a digital circuit with a set of components C       */
/*  2. a function f computed by the circuit                                */
/*  3. a symptom consisting of an input output pair (i,o) such that        */
/*      f(i) <> o                                                          */
/* Find:                                                                   */
/*   a diagnosis D. D is a subset of C which, if not working correctly,    */
/*   could result in the circuit computing o given i.                      */
/*                                                                         */
/* The specific circuit used for this benchmark is an N bit adder with     */
/* forward carry propagation.  However, any combinatorial circuit diagnosis*/
/* problem could easily formulated from it's network description.          */
/* This example was constructed based on an example from an article about  */
/* Prolog III in CACM July 1990.                                           */
/* The problem consists in finding the minimum number of broken components */
/* in a N bit adder that thinks 0+0=2^N-1 (the answer is always N).        */
/* Each adder consists of 5 gates (2 'and', 2 'xor' and 1 'or').           */
/* A boolean (Di) is associated to each gate and it is true (1) if the     */
/* gate is broken. The solution is a list of Di. F is the number of broken */
/* components. To minimize F we label it (indomain) first (since there is  */
/* no choice point it is correct).                                         */
/*                                                                         */
/* Solution:                                                               */
/* N=1 [0,0,0,0,1]                                                         */
/* N=2 [0,0,0,0,1,0,0,0,0,1]                                               */
/* N=3 [0,0,0,0,1,0,0,0,0,1,0,0,0,0,1]                                     */
/*-------------------------------------------------------------------------*/

q :-
	statistics(runtime, _),
	write('N ?'),
	read_integer(N),
	Z is 1 << N - 1,
	bdiag(N, 0, 0, Z, 0, 0, Ds, F),
	statistics(runtime, [_, Y]),
	write(s(F, Ds)),
	nl,
	write('time : '),
	write(Y),
	nl.




bdiag(N, X, Y, Z, C1, C, Ds, F) :-
	N5 is N * 5,
	F #=< N5,
	nadder(N, X, Y, Z, C1, C, Ds),
	TN is 1 << N,
	X + Y + C1 #\= Z + TN * C,
	sum(Ds, F),
	fd_minimize(fd_labeling(Ds), F).
%	fd_labeling([F|Ds]).




sum([], 0).
sum([X|Xs], S) :-
	S #= X + S1,
	sum(Xs, S1).




nadder(N, X, Y, Z, C1, C, Ds) :-
	bits(N, X, Xs),
	bits(N, Y, Ys),
	bits(N, Z, Zs),
	adder(Xs, Ys, Zs, C1, C, Ds).




bits(N, X, Xs) :-
	length(Xs, N),
	bits1(Xs, 0, N, X).




bits1([], N, N, 0).

bits1([Xi|Xs1], I, N, X) :-
	I < N,
	X #= Xi * 2 ** I + X1,
	I1 is I + 1,
	bits1(Xs1, I1, N, X1).




adder([], [], [], C, C, []).

adder([X|Xs], [Y|Ys], [Z|Zs], C1, C, [D0, D1, D2, D3, D4|Ds]) :-
	fullAdder(X, Y, C1, Z, C2, D0, D1, D2, D3, D4),
	adder(Xs, Ys, Zs, C2, C, Ds).




fullAdder(X, Y, C1, Z, C, D0, D1, D2, D3, D4) :-
	#\ D0 #==> (U1 #<=> X #/\ Y),
	#\ D1 #==> (U2 #<=> U3 #/\ C1),
	#\ D2 #==> (C #<=> U1 #\/ U2),
	#\ D3 #==> (U3 #<=> X ## Y),
	#\ D4 #==> (Z #<=> U3 ## C1).




:-	initialization(q).
