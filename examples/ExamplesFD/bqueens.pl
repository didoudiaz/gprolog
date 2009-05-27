/*-------------------------------------------------------------------------*/
/* Benchmark (Boolean)                                                     */
/*                                                                         */
/* Name           : bqueens.pl                                             */
/* Title          : N-queens problem                                       */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : January 1993                                           */
/*                                                                         */
/* Put N queens on an NxN chessboard so that there is no couple of queens  */
/* threatening each other.                                                 */
/* The solution is a list [ [Que11,...,Que1N], ... ,[QueN1,...,QueNN] ]    */
/* where Queij is 1 if the the  is a queen on the ith line an jth row.     */
/*                                                                         */
/* Solution:                                                               */
/* N=4  [[0,0,1,0],         [[0,1,0,0],                                    */
/*       [1,0,0,0],          [0,0,0,1],                                    */
/*       [0,0,0,1],  and     [1,0,0,0],                                    */
/*       [0,1,0,0]]          [0,0,1,0]]                                    */
/*                                                                         */
/* N=8  [[0,0,0,0,0,0,0,1], (first solution)                               */
/*       [0,0,0,1,0,0,0,0],                                                */
/*       [1,0,0,0,0,0,0,0],                                                */
/*       [0,0,1,0,0,0,0,0],                                                */
/*       [0,0,0,0,0,1,0,0],                                                */
/*       [0,1,0,0,0,0,0,0],                                                */
/*       [0,0,0,0,0,0,1,0],                                                */
/*       [0,0,0,0,1,0,0,0]]                                                */
/*-------------------------------------------------------------------------*/

q :-
	write('N ?'),
	read_integer(N),
	statistics(runtime, _),
	(   bqueens(N, A),
	    write(A),
	    nl                                                             %,
%        fail
	;   write('No more solutions'),
	    nl
	),
	statistics(runtime, [_, Y]),
	write('time : '),
	write(Y),
	nl.




bqueens(N, A) :-
	create_array(N, N, A),
	for_each_line(A, only1),
	for_each_column(A, only1),
	for_each_diagonal(A, N, N, atmost1), !,
	array_labeling(A).





:-	include(array).

% interface with for_each_... procedures

array_prog(only1, L) :-
	fd_only_one(L).

array_prog(atmost1, L) :-
	fd_at_most_one(L).




:-	initialization(q).
