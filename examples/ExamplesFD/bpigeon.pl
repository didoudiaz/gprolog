/*-------------------------------------------------------------------------*/
/* Benchmark (Boolean)                                                     */
/*                                                                         */
/* Name           : bpigeon.pl                                             */
/* Title          : pigeon-hole problem                                    */
/* Originated from:                                                        */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : January 1993                                           */
/*                                                                         */
/* Put N pigeons in M pigeon-holes. Solution iff N<=M.                     */
/* The solution is a list [ [Pig11,...,Pig1m], ... ,[Pign1,...,Pignm] ]    */
/* where Pigij = 1 if the pigeon i is in the pigeon-hole j                 */
/*                                                                         */
/* Solution:                                                               */
/* N=2 M=3 [[0,0,1],[0,1,0]]                                               */
/*         [[0,0,1],[1,0,0]]                                               */
/*         [[0,1,0],[0,0,1]]                                               */
/*         [[0,1,0],[1,0,0]]                                               */
/*         [[1,0,0],[0,0,1]]                                               */
/*         [[1,0,0],[0,1,0]]                                               */
/*-------------------------------------------------------------------------*/

q :-
	write('N ?'),
	read_integer(N),
	write('M ?'),
	read_integer(M),
	statistics(runtime, _),
	g_assign(count, 0),
	(   bpigeon(N, M, _A),
%        write(_A), nl,
	    g_inc(count),
	    fail
	;
	    g_read(count, Count),
	    format('Number of solutions ~d~n', [Count])
	),
	statistics(runtime, [_, Y]),
	write('time : '),
	write(Y),
	nl.




bpigeon(N, M, A) :-
	create_array(N, M, A),
	for_each_line(A, only1),
	for_each_column(A, atmost1), !,
	array_labeling(A).



:-	include(array).

% interface with for_each_... procedures

array_prog(only1, L) :-
	fd_only_one(L).

array_prog(atmost1, L) :-
	fd_at_most_one(L).




:-	initialization(q).
