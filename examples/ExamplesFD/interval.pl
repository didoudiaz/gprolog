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
/* Solution:                                                               */
/* N=8  [0,7,1,6,2,5,3,4]                                                  */
/* N=14 [0,13,1,12,2,11,3,10,4,9,5,8,6,7]                                  */
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
	
	% avoid dual solution (symmetry)
	LD = [D1|_],
	last(LD, D2),
	D1 #> D2,
	
	% the labeling of LD speeds up a lot if just the first solution is wanted (else remove it)
	fd_labeling(LD, [value_method(max)]),

	% the labeling (useless if only the first solution is wanted, labeling of LD is enough)
	fd_labeling(L, [variable_method(ff), value_method(middle)]).




mk_dist([], _, []).

mk_dist([Y|L], X, [D|LD]) :-
	D #= dist(X, Y),
	mk_dist(L, Y, LD).


:-	initialization(q).
