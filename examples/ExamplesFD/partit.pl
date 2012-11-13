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
/* Two redundant constraints are used:                                     */
/*                                                                         */
/*   - in order to avoid duplicate solutions (permutations) we impose      */
/*     A1<A2<....<AN/2, B1<B2<...<BN/2 and A1=1. This achieves much more   */
/*     pruning than only one fd_all_different constraint.                  */
/*                                                                         */
/*   - the half sums are known                                             */
/*                              N                                          */
/*        Sum k^1 = Sum l^1 = (Sum i) / 2 = N*(N+1) / 2 / 2                */
/*       k in A    l in B      i=1                                         */
/*                              N                                          */
/*        Sum k^2 = Sum l^2 = (Sum i^2)/2 = N*(N+1)*(2*N+1) / 6 / 2        */
/*       k in A    l in B      i=1                                         */
/*                                                                         */
/* The labeling heuristics consists in placing the biggest missing value   */
/* (from N to 1). If only one solution is needed, it is better for first   */
/* to put this value in the group which has the smallest sum (of already   */
/* placed values). If all solutions are wanted this is not relevant and    */
/* incurs an little overhead.                                              */
/*                                                                         */
/* Generalization: finding a partition of 1,2...,N into 2 groups A and B:  */
/*                                                                         */
/*     Sum (x^k) = Sum y^k                                                 */
/*   x in A      y in B                                                    */
/*                                                                         */
/* Condition a) is a special case where k=0, b) where k=1 and c) where k=2.*/
/*                                                                         */
/* Solution:                                                               */
/*                                                                         */
/* N=8  A=[1,4,6,7]                                                        */
/*      B=[2,3,5,8]                                                        */
/*                                                                         */
/* N=16 A=[1,4,6,7,10,11,13,16]                                            */
/*      B=[2,3,5,8,9,12,14,15]                                             */
/*                                                                         */
/* N=20 A=[1,3,7,8,9,11,14,15,17,20]                                       */
/*      B=[2,4,5,6,10,12,13,16,18,19]                                      */
/*                                                                         */
/* N=24 A=[1,5,6,7,8,12,13,16,17,20,21,24]                                 */
/*      B=[2,3,4,9,10,11,14,15,18,19,22,23]                                */
/*                                                                         */
/* Computing all solutions                                                 */
/*                                                                         */
/* N=8            1 solutions in      0.00 secs = 0ms                      */
/* N=12           1 solutions in      0.00 secs = 0ms                      */
/* N=16           7 solutions in      0.01 secs = 10ms                     */
/* N=20          24 solutions in      0.01 secs = 10ms                     */
/* N=24         296 solutions in      0.03 secs = 30ms                     */
/* N=28        1443 solutions in      0.35 secs = 350ms                    */
/* N=32       17444 solutions in      3.51 secs = 3s 510ms                 */
/* N=36      138905 solutions in     35.86 secs = 35s 860ms                */
/* N=40     1581207 solutions in    385.07 secs = 6m 25s 70ms              */
/* N=44    14762400 solutions in   4222.02 secs = 1h 10m 22s 20ms          */
/* N=48   176977514 solutions in  48276.96 secs = 13h 24m 36s 960ms        */
/* N=52  1850331835 solutions in 552017.03 secs = 6d 9h 20m 17s 30ms       */
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
	init_group(N, A),
	init_group(N, B),
	A = [1|_],		% fix 1 as the first value of the group A
	cstr_pow(1, N, A, B),
	cstr_pow(2, N, A, B),
%	cstr_pow(3, N, A, B),	% uncomment to add ^3 constraints
%	cstr_pow(4, N, A, B),	% uncomment to add ^4 constraints
	reverse(A, AR),
	reverse(B, BR),
	enum_one(N, 0, 0, AR, BR).   % better if only one solution is wanted
%	enum_all(N, AR, BR).	% a bit better if all solutions are wanted



compute_and_check_half_sum(_N, _P, S, HS) :-
	S mod 2 =:= 0, !,
	HS is S // 2.

compute_and_check_half_sum(N, P, S, _) :-
	format('failure since sum of power ~d until ~d = ~d is odd (half-sum is not integer)~n', [P, N, S]),
	fail.



init_group(N, L) :-
	compute_and_check_half_sum(N, 0, N, N2),
	length(L, N2),
	fd_domain(L, 1, N),
	ascending_order(L).



ascending_order([X|L]) :-
	ascending_order(L, X).


ascending_order([], _).

ascending_order([Y|L], X) :-
	Y #> X,
	ascending_order(L, Y).



cstr_pow(P, N, A, B) :-
	sum_power(P, N, S),
	compute_and_check_half_sum(N, P, S, HS),
	cstr_pow(A, P, HS),
	cstr_pow(B, P, HS).



cstr_pow([], _, 0).

cstr_pow([X|L], P, S) :-
	X ** P #= XP,
	S #= XP + S1,
	cstr_pow(L, P, S1).




/* Known sums of powers
 * sum of n first integers: s1(n) = n * (n+1) / 2
 * sum of n first squares : s2(n) = n * (n+1) * (2*n+1)/ 6
 * sum of n first cubes   : s3(n) = n^2 * (n+1)^2 / 4 = s1(n)^2
 * sum of n fisrt pow 4   : s4(n) = n * (n+1) * (6*n^3 + 9*n^2 + n - 1 ) / 30
 */


sum_power(1, N, S) :-
	S is N * (N + 1) // 2.

sum_power(2, N, S) :-
	S is  N * (N + 1) * (2 * N + 1) // 6.

sum_power(3, N, S) :-
	sum_power(1, N, S2),
	S is S2 * S2.

sum_power(4, N, S) :-
	S is N * (N+1) * (6*N^3 + 9*N^2 + N - 1) // 30.



/* The labeling heuristics consists in placing the biggest missing value (from N to 1) */


enum_all(1, _, _) :-
	!.

enum_all(N, [N|A], B) :-
	N1 is N - 1,
	enum_all(N1, A, B).

enum_all(N, A, [N|B]) :-
	N1 is N - 1,
	enum_all(N1, A, B).

/* If only one solution is wanted, it is better to first try to put the biggest missing value
 * in the group which has the smallest sum (of already placed values). */

enum_one(1, _, _, _, _) :-
	!.

enum_one(N, SumA, SumB, A, B) :-
	SumA > SumB, !,
	enum_one(N, SumB, SumA, B, A).

enum_one(N, SumA, SumB, [N|A], B) :- 		% in A first (which has the smallest sum) then...
	SumA1 is SumA + N,
	N1 is N - 1,
	enum_one(N1, SumA1, SumB, A, B).

enum_one(N, SumA, SumB, A, [N|B]) :- 		% in B at backtracking
	SumB1 is SumB + N,
	N1 is N - 1,
	enum_one(N1, SumA, SumB1, A, B).


:-	initialization(q).




%%% to compute the number of solutions


all(N) :-
	g_assign(nb,0),
	user_time(T0),
	all(N, T0).


all(N, T0) :-
	partit(N, _, _),
	g_inc(nb, NB),
	NB mod 100000 =:= 0,	% adapt this to have more or less displayed lines
	show_time(NB, T0),
	fail.


all(N, T0) :-
	g_read(nb, NB),
	format('\nfinal for partit ~d:\n\n', [N]),
	show_time(NB, T0).



show_time(NB, T0) :-
	user_time(T1),
	T is (T1 - T0),
	format('%10d solutions in ', [NB]),
	disp_time(T),
	TA is T / NB,
	write('\n           average      '),
	disp_time(TA),
	write(' / sol\n').


disp_time(T) :-
	T1 is T / 1000,
	format('%20.6f secs =', [T1]),
	disp_time([86400000-d,3600000-h,60000-m,1000-s,1-ms],
		  T, nothing_yet_displayed).




disp_time([], T, nothing_yet_displayed) :-
	!,
	format(' %.3f ms', [T]).

disp_time([], _, _).


disp_time([M-_|LM], T, nothing_yet_displayed) :-
	T < M, !,
	disp_time(LM, T, nothing_yet_displayed).

disp_time([M-U|LM], T, _) :-
	N is truncate(T / M),
	T1 is T - (N * M),
	format(' ~d~a', [N, U]),
	disp_time(LM, T1, something_is_displayed).






