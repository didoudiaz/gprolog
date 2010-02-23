/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : partit.pl                                              */
/* Title          : integer partitionning                                  */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : September 1993 (modified March 1997, feb 2010)         */
/*                                                                         */
/* Partition numbers 1,2,...,N into two groups A and B such that:          */
/*   a) A and B have the same length,                                      */
/*   b) sum of numbers in A = sum of numbers in B,                         */
/*   c) sum of squares of numbers in A = sum of squares of numbers in B.   */
/*                                                                         */
/* It seems there is a solution if N >= 8 and N is a multiple of 4.        */
/*                                                                         */
/* Generalization: finding a partition of 1,2...,N into 2 groups A and B:  */
/*                                                                         */
/*     Sum (k^p) = Sum l^p                                                 */
/*   k in A      l in B                                                    */
/*                                                                         */
/* Condition a) is a special case where p=0, b) where p=1 and c) where p=2.*/
/*                                                                         */
/* Two redundant constraints are used:                                     */
/*                                                                         */
/*   - in order to avoid duplicate solutions (permutations) we impose      */
/*     A1<A2<....<AN/2, B1<B2<...<BN/2 and A1=1. This achieves much more   */
/*     pruning than only one fd_all_different constraint.                  */
/*                                                                         */
/*   - the half sums are known                                             */
/*                              N                                          */
/*        Sum k^1 = Sum l^1 = (Sum i) / 2 = N*(N+1) / 4                    */
/*       k in A    l in B      i=1                                         */
/*                              N                                          */
/*        Sum k^2 = Sum l^2 = (Sum i^2)/2 = N*(N+1)*(2*N+1) / 12           */
/*       k in A    l in B      i=1                                         */
/*                                                                         */
/* Solution:                                                               */
/*                                                                         */
/* N=8  A=[1,4,6,7]                                                        */
/*      B=[2,3,5,8]                                                        */
/*                                                                         */
/* N=16 A=[1,4,5,8,10,11,14,15]                                            */
/*      B=[2,3,6,7,9,12,13,16]                                             */
/*                                                                         */
/* N=20 A=[1,4,5,7,11,12,13,15,18,19]                                      */
/*      B=[2,3,6,8,9,10,14,16,17,20]                                       */
/*                                                                         */
/* N=24 A=[1,3,6,8,10,11,14,15,18,19,22,23]                                */
/*      B=[2,4,5,7,9,12,13,16,17,20,21,24]                                 */
/*-------------------------------------------------------------------------*/


q :-
	write('N ?'),
	read_integer(N),
	statistics(runtime, _),
	partit(N, A, B),
	statistics(runtime, [_, Y]),
	write(sol(A, B)),
	nl,
	write('time : '),
	write(Y),
	nl.




partit(N, A, B) :-
	N2 is N // 2,
	length(A, N2),
	fd_domain(A, 1, N),
	length(B, N2),
	fd_domain(B, 1, N),
	no_duplicate(A, B),
	half_sums(N, HS1, HS2),
	sums(A, HS1, HS2),
	sums(B, HS1, HS2),	% redundant but more efficient
	reverse(A, AR),
	reverse(B, BR),
	enum(N, 0, 0, AR, BR).


sums([], 0, 0).

sums([X|L], S1, S2) :-
	sums(L, T1, T2),
	X ** 2 #= X2,
	S1 #= X + T1,
	S2 #= X2 + T2.




no_duplicate(A, B) :-
	ascending_order(A),
	ascending_order(B),	% redundant but more efficient
	A = [1|_].





ascending_order([X|L]) :-
	ascending_order(L, X).


ascending_order([], _).

ascending_order([Y|L], X) :-
	Y #> X,
	ascending_order(L, Y).



half_sums(N, HS1, HS2) :-
	S1 is N * (N + 1) // 2,
	S2 is S1 * (2 * N + 1) // 3,
	HS1 is S1 // 2,
	HS2 is S2 // 2 .



/* the labeling heuristics consists in placing the biggest missing value
 * (from N to 1) in the group which has the smallest sum first */


enum(N, SumA, SumB, A, B) :-
	N > 1, !,
	(   SumA < SumB ->
	    (   enum1(N, SumA, SumB, A, B)  % in A then (at backtracking) in B
	    ;
		enum1(N, SumB, SumA, B, A)
	    )
	;
	    (   enum1(N, SumB, SumA, B, A)  % in B then (at backtracking) in A
	    ;
		enum1(N, SumA, SumB, A, B)
	    )
	).

enum(_, _, _, _, _).


enum1(N, SumA, SumB, [N|A], B) :-
	SumA1 is SumA + N,
	N1 is N - 1,
	enum(N1, SumA, SumB, A, B).






:-	initialization(q).
