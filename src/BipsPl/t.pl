/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : development only                                                *
 * File  : t.pl                                                            *
 * Descr.: test - Prolog part                                              *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2022 Daniel Diaz                                     *
 *                                                                         *
 * GNU Prolog is free software; you can redistribute it and/or modify it   *
 * under the terms of the GNU General Public License as published by the   *
 * Free Software Foundation; either version 2, or any later version.       *
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful, but       *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received a copy of the GNU General Public License along *
 * with this program; if not, write to the Free Software Foundation, Inc.  *
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               *
 *-------------------------------------------------------------------------*/

/*
 * You can put your own test code in these files (see DEVELOPMENT)
 *    t.pl    (Prolog part)
 *    t_c.c   (C part, eg. foreign code or C code used by your FD constraints)
 *    t_fd.fd (FD constraint part)
 */

/*
ind(X) :-
	fd_min(X, Min),
	X #= Min.

ind(X) :-
	write(back(X)),nl,
	fd_min(X, Min),
	X #> Min,
	ind(X).
*/
	


/*
%:- initialization(z1).

z1:- fd_domain(X,[62,63,64,65,66,67,68,69,70]), fd_size(X,N), write(N), nl, halt.


z0:- catch(z, _, write('TOO BIG\n')), halt.

z:-
        Z = [A, B, C, D, E, F, G, H, I, J, K, L],
        fd_domain_bool(Z),      
%        A ## B ## C.
        A ## B ## C ## D ## E ## F ## G ## H ## I ## J ## K ## L.
*/


/*
works(L) :-
        L = [Z1, Z2],
        fd_domain(L, 10, 99),
        fd_prime(Z1),
        cross_sum(Z1, Z2),
        fd_labeling(L),
        is_square(Z2)
        .

broken(L) :-
        L = [Z1, Z2],
        fd_domain(L, 10, 99),
        fd_prime(Z1),
        cross_sum(Z1, Z2),
        is_square(Z2),
        fd_labeling(L).


cross_sum(X, X) :- X #< 10.
cross_sum(X, Y) :- X #> 9, Y1 #= X rem 10, X1 #= X // 10,
cross_sum(X1, Z), Y #= Z + Y1.

is_square(1).
is_square(4).
is_square(9).
is_square(X) :- Y #>= 1, Y #=< X, X #= Y * Y.
*/

/*
sum([], 0):- statistics.
sum([X|L], S1) :-
	sum(L, S),
	S1 #= X + S.

sum1([], 0):- statistics. 
sum1([X|L], S1) :-
	S1 #= X + S,
	sum1(L, S).

p :- length(BL, 10000), sum(BL, _BS), statistics.
p1 :- length(BL, 10000), sum1(BL, _BS), statistics.


dle(S1, S2, D, SY) :-
	fd_tell(dist_le(S1, S2, D, SY)).

*/
/*
dle(X) :-
	fd_tell(foo(X)).
*/
/*

bug(L) :-
	L=[P1,P2,P3,P4,P6,P7,P8,P9],
	fd_domain(L,[2,3,5,7]),
%	uncomment the following line and this works correctly
%	P2#=7,
	P4 * (100 * P3 + 10 * P2 + P1 )	#= 1000 * P9 + 100 * P8 + 10 * P7 + P6.
*/
/*
q :-
	LD = [S, E, N, D],
	fd_domain(LD, 0, 4),
	fd_all_different(LD),
	S + 3 * E #= U, U #= 5 * N + D,
%	S=0, write(LD1), nl,
%	E=3, write(LD1), nl,  % E=2 ne marche pas + remettre optim2 + SEH dans chkma
%	N=1, write(LD), nl,
	fd_labeling(LD),
	write(LD),
	nl.


a:- q, fail ; true.

:-	initialization(a).
*/
/*
condition_opaque_to_cut_3(1) :-
	(   ! *-> 
	    true
	;   fail
	).
condition_opaque_to_cut_3(2).
*/
/*
soft(1) :-
	(   ! *-> 
	    write(a)
	;   fail
	).
soft(2).

hard(1) :-
	(   ! -> 
	    true
	;   fail
	).
hard(2).


q :- soft(X), write(X), nl, fail.
q.

:- initialization(q).

*/
/*
setup_call_cleanup(Setup, Goal, Cleanup) :-
	set_bip_name(setup_call_cleanup, 3),
	call(Setup), !,
	(   var(Cleanup) ->
	    '$pl_err_instantiation'
	;
	    callable(Cleanup) ->
	    true
	;
	    '$pl_err_type'(callable, Cleanup)
	),
        catch('$call_det'(Goal, Det), Ball, true),
	(   Det == true, !,
	    '$scc_exec_cleanup'(Cleanup)
	;
	    nonvar(Ball), !,
	    '$scc_exec_cleanup_and_throw'(Cleanup, Ball)
	;
	    true % some choice-points remain, cleanup not yet executed (must be suspended)
	).

'$scc_exec_cleanup'(Cleanup) :-
	'$call'(Cleanup, setup_call_cleanup, 3, true), !.

'$scc_exec_cleanup'(_Cleanup).


'$scc_exec_cleanup_and_throw'(Cleanup, Ball) :-
	'$catch'('$scc_exec_cleanup'(Cleanup), Ball1, '$scc_exec_cleanup_raised'(Ball1), setup_call_cleanup, 3, true),
	throw(Ball).


'$scc_exec_cleanup_raised'(Error) :-
	Error = error(_, setup_call_cleanup/3),
	!,
	throw(Error).

'$scc_exec_cleanup_raised'(_).


p(_,_):-call(_).
*/




/*

lgt_current_output(S) :-
	current_output(S), !.

lgt_current_output(S) :-
	current_stream(S), !,
	fail.

lgt_current_output(S) :-
	set_bip_name(current_output, 1),
	'$pl_err_existence'(stream, S).

*/


:- multifile(p/0).
:- dynamic(p/0).

p.
p:-write(toto),nl.


%:- initialization((trace, p)).
%foo:-write(a),write(b),nl.
