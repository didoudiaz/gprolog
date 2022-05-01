/*-------------------------------------------------------------------------*/
/* Benchmark (Boolean)                                                     */
/*                                                                         */
/* Name           : bramsey.pl                                             */
/* Title          : ramsey problem                                         */
/* Original Source: Daniel Diaz - INRIA France                             */
/*                  Greg Sidebottom - University of Vancouver Canada       */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : September 1993                                         */
/*                                                                         */
/* Find a 3-colouring of a complete graph with N vertices such that there  */
/* is no monochrome triangles.                                             */
/*                                                                         */
/* The graph is a half-matrix of edges. Example N=5:                       */
/* Graph=m(v(e12),                                                         */
/*         v(e13, e23),                                                    */
/*         v(e14, e24, e34),                                               */
/*         v(e15, e25, e35, e45)) an edge eij is  3 colors [C3,C2,C1]      */
/* (resolution by line)                                                    */
/*                                                                         */
/* There is a solution up to N=16, none for N>=17.                         */
/* Solution:                                                               */
/* N=5                                                                     */
/* m(v([0,0,1]),                                                           */
/*   v([0,1,0],[0,0,1]),                                                   */
/*   v([0,1,0],[0,0,1],[1,0,0]),                                           */
/*   v([1,0,0],[0,0,1],[0,1,0],[0,1,0]))                                   */
/*-------------------------------------------------------------------------*/

q :-
	write('N ?'),
	read_integer(N),
	statistics(runtime, _),
	ramsey(N, Graph),
	statistics(runtime, [_, Y]),
	write(Graph),
	nl,
	write('time : '),
	write(Y),
	nl.




ramsey(N, Mat) :-
	adj(N, Mat),
	triangles(N, Mat, Tris),
	label(Tris).




triangles(N, Mat, Ts) :-
	trianglesI(0, N, Mat, Ts, []).




trianglesI(I1, N, Mat, Ts1, Ts) :-
	I1 < N, !,
	I is I1 + 1,
	trianglesJI(I, I, N, Mat, Ts1, Ts2),
	trianglesI(I, N, Mat, Ts2, Ts).

trianglesI(N, N, _Mat, Ts, Ts).




trianglesJI(J1, I, N, Mat, Ts1, Ts) :-
	J1 < N, !,
	J is J1 + 1,
	trianglesKJI(J, J, I, N, Mat, Ts1, Ts2),
	trianglesJI(J, I, N, Mat, Ts2, Ts).

trianglesJI(N, _I, N, _Mat, Ts, Ts).




trianglesKJI(K1, J, I, N, Mat, [EIJ, EJK, EKI|Ts1], Ts) :-
	K1 < N, !,
	K is K1 + 1,
	edge(I, J, Mat, EIJ),
	edge(J, K, Mat, EJK),
	edge(I, K, Mat, EKI),
	polychrom(EIJ, EJK, EKI),
	trianglesKJI(K, J, I, N, Mat, Ts1, Ts).

trianglesKJI(N, _J, _I, N, _Mat, Ts, Ts).




polychrom([C13, C12, C11], [C23, C22, C21], [C33, C32, C31]) :-
	#\  (C13 #/\ C23 #/\ C33),
	#\  (C12 #/\ C22 #/\ C32),
	#\  (C11 #/\ C21 #/\ C31).




% these interface to the tmat routines, the essentially map the matrix
% so the diagonal can be used

adj(N, Mat) :-
	N1 is N - 1,
	tmat(N1, Mat).




% edge must be called with I < J
% could make more general so it swaps arguments if I > J

edge(I, J, Mat, EIJ) :-
	J1 is J - 1,
	tmatRef(J1, I, Mat, EIJ),
	(   var(EIJ) ->
	    cstr_edge(EIJ)
	;   true
	).



tmat(N, Mat) :-
	functor(Mat, m, N),
	tvecs(N, Mat).




tvecs(0, _Mat) :-
	!.

tvecs(J, Mat) :-
	arg(J, Mat, Vec),
	functor(Vec, v, J),
	J1 is J - 1,
	tvecs(J1, Mat).




% tmatRef must be called with I > J
% could make more general so it swaps arguments if I < J

tmatRef(I, J, Mat, MatIJ) :-
	arg(I, Mat, MatI),
	arg(J, MatI, MatIJ).




label([]).

label([A, B, C|L]) :-
	labeltri(A, B, C),
	label(L).




labeltri(A, B, C) :-
	same_edge(A, B),
	fd_labeling(A),
	fd_labeling(C).

labeltri(A, B, C) :-
	same_edge(A, C),
	fd_labeling(A),
	fd_labeling(B).

labeltri(A, B, C) :-
	same_edge(B, C),
	fd_labeling(B),
	fd_labeling(A).

labeltri(A, B, C) :-
	fd_labeling(C),
	diff_edge(A, C),
	diff_edge(B, C),
	fd_labeling(B),
	diff_edge(A, B).




same_edge(Edge, Edge).




diff_edge([C13, C12, C11], [C23, C22, C21]) :-
	#\  (C13 #/\ C23),
	#\  (C12 #/\ C22),
	#\  (C11 #/\ C21).




cstr_edge(E) :-
	E = [_, _, _],
	fd_only_one(E).




:-	initialization(q).
