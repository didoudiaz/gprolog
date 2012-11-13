/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : all-interval.pl                                        */
/* Title          : all-interval series problem                            */
/* Original Source:                                                        */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : May 2009                                               */
/*                                                                         */
/* Find sequence of N different values in 0 .. N-1 such that the distance  */
/* between 2 consecutive values are all distinct.                          */
/*                                                                         */
/* NB: there is an obvious solution: 0 N-1  1 N-2  N N-3                   */
/* this solution is found without backtracking with a labeling on distances*/
/* enumerating variables from their max to the min (see labeling on LD)    */
/* For other solutions, remove this labeling.                              */
/*                                                                         */
/* Solution:                                                               */
/* N=8  [1,7,0,5,2,6,4,3]                                                  */
/* N=14 [1,13,0,11,2,12,4,10,3,8,5,9,7,6]                                  */
/*-------------------------------------------------------------------------*/


q :-
	write('N ?'),
	read_integer(N),
	statistics(runtime, _),
	interval(N, L),
	statistics(runtime, [_, Y]),
	write(L),
	nl,
	write('time : '),
	write(Y),
	nl.




interval(N, L) :-
	N1 is N - 1,
	fd_set_vector_max(N),
	length(L, N),
	fd_domain(L, 0, N1),
	L = [X|L1],
	mk_dist(L1, X, LD),
	fd_domain(LD, 1, N1),
	fd_all_different(L),
	fd_all_different(LD),

	% avoid mirror symmetry
	L = [X, Y|_],
	X #< Y,
	X #> 0,

	% avoid dual solution (symmetry)
	LD = [D1|_],
	last(LD, D2),
	D1 #> D2,

	% the labeling of LD speeds up a lot if just the first solution is wanted (else remove it)
	fd_labeling(LD, [value_method(max), backtracks(_B)]),
	%write(_B), nl,

	% the labeling (useless if only the first solution is wanted, labeling of LD is enough)
	fd_labeling(L, [variable_method(ff), value_method(middle)]).




mk_dist([], _, []).

mk_dist([Y|L], X, [D|LD]) :-
	D #= dist(X, Y),
	mk_dist(L, Y, LD).


:-	initialization(q).
