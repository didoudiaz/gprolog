/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : partit.pl                                              */
/* Title          : integer partitionning                                  */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : September 1993 (modified March 1997)                   */
/*                                                                         */
/* Partition numbers 1,2,...,N into two groups A and B such that:          */
/*   a) A and B have the same length,                                      */
/*   b) sum of numbers in A = sum of numbers in B,                         */
/*   c) sum of squares of numbers in A = sum of squares of numbers in B.   */
/*                                                                         */
/* This problem admits a solution if N is a multiple of 8.                 */
/*                                                                         */
/* Note: finding a partition of 1,2...,N into 2 groups A and B such that:  */
/*                                                                         */
/*     Sum (k^p) = Sum l^p                                                 */
/*   k in A      l in B                                                    */
/*                                                                         */
/* admits a solution if N mod 2^(p+1) = 0 (N is a multiple of 2^(p+1)).    */
/* Condition a) is a special case where p=0, b) where p=1 and c) where p=2.*/
/*                                                                         */
/* Two redundant constraints are used:                                     */
/*                                                                         */
/*   - in order to avoid duplicate solutions (permutations) we impose      */
/*     A1<A2<....<AN/2, B1<B2<...<BN/2 and A1<B1. This achieves much more  */
/*     pruning than only fd_all_differents(A) and fd_all_differents(B).    */
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
/* N=8  A=[1,4,6,7]                                                        */
/*      B=[2,3,5,8]                                                        */
/*                                                                         */
/* N=16 A=[1,3,6,8,10,12,13,15]                                            */
/*      B=[2,4,5,7,9,11,14,16]                                             */
/*                                                                         */
/* N=24 A=[1,2,6,8,10,12,15,16,17,19,21,23]                                */
/*      B=[3,4,5,7,9, 11,13,14,18,20,22,24]                                */
/*                                                                         */
/* N=32 A=[1,2,3,7,13,14,15,16,18,19,21,23,25,27,29,31]                    */
/*      B=[4,5,6,8, 9,10,11,12,17,20,22,24,26,28,30,32]                    */
/*                                                                         */
/* N=40 A=[1,2,3,4,12,15,16,18,19,20,21,23,25,27,29,31,33,35,37,39]        */
/*      B=[5,6,7,8, 9,10,11,13,14,17,22,24,26,28,30,32,34,36,38,40]        */
/*-------------------------------------------------------------------------*/


q:-	write('N ?'), read_integer(N),
	statistics(runtime,_),
	partit(N,A,B), statistics(runtime,[_,Y]),
	write(sol(A,B)), nl,
	write('time : '), write(Y), nl.




partit(N,A,B):-
	fd_set_vector_max(N),
	N2 is N//2,
	length(A,N2),
	fd_domain(A,1,N),
	length(B,N2),
	fd_domain(B,1,N),
	merge_and_reverse(A,B,[],L),                 % best order for label
	fd_all_different(L),
	no_duplicate(A,B),                           % redundant constraint 1
	half_sums(N,HS1,HS2),                        % redundant constraint 2
	sums(A,HS1,HS2),
	sums(B,HS1,HS2),
	fd_labeling(L,[value_method(max)]).              % best value heuristics



merge_and_reverse([],_,Acc,Acc).

merge_and_reverse([X1|L1],[X2|L2],Acc,L):-
	merge_and_reverse(L1,L2,[X2,X1|Acc],L).



sums([],0,0).

sums([X|L],S1,S2):-
	sums(L,T1,T2),
	X**2#=X2,
	S1#=X+T1,
	S2#=X2+T2.




no_duplicate(A,B):-
	ascending_order(A),
	ascending_order(B),
	A=[X1|_],
	B=[X2|_],
	X2 #> X1.




ascending_order([X|L]):-
	ascending_order(L,X).


ascending_order([],_).

ascending_order([Y|L],X):-
	Y #> X,
	ascending_order(L,Y).



half_sums(N,HS1,HS2):-
	S1  is N*(N+1)//2,
	S2  is S1*(2*N+1)//3,
	HS1 is S1//2,           
	HS2 is S2//2.




:- initialization(q).

