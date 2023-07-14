/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : development only                                                *
 * File  : t.pl                                                            *
 * Descr.: test - Prolog part                                              *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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

:- if(fail).
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

:- multifile(p/0).
:- dynamic(p/0).

p.
p:-write(toto),nl.


%:- initialization((trace, p)).

*/
%foo:-write(a),write(b),nl. % %

/*
test(Goal) :-
	open('/tmp/foo.out', write, Stream),
	set_output(Stream),
	set_stream_alias(Stream, user_error),
%	set_error(Stream),
	% redirect any output from Goal using implicit streams to file "foo"
	call(Goal),
	close(Stream).

tt :-	test((write(abc),nl,write(user_error,toto),nl)).



f :- open('/tmp/foo', read, X, [alias(foo), buffering(none)]), write(X-foo), nl.
b :- open('/tmp/bar', write, X, [alias(bar), buffering(none)]), write(X-bar), nl.
b1 :- open('/tmp/bar1', write, X, [alias(bar1), buffering(none)]), write(X-bar1), nl.


z :-	f, b, b1, exec('sort', foo, bar, bar1), closeall.


z2 :-
	exec('sort', X, Y, Z, PID), 
	write(X, 'b\na\nz\ng'), 
	close(X), r(Y), r(Z), 
	wait(PID, _), 
	closeall.

z3 :-
	exec('du -s /var/log', _, Y, Z), r(Y), r(Z), closeall.

z4 :-
	b, 
	Y=bar, 
	Z=bar, 
	exec('sort', X, Y, Z, PID), 
	write(executing(PID)), nl, 
	open('~/wordnet-prolog/wn_valid.pl', read, _, [alias(w)]), 
	repeat, 
	get_char(w, C),
	(   C == end_of_file ; write(X, C), fail ), !, 
	closeall, 
	wait(PID, _).


r(S) :-
	write('--------------------------'(S)), nl, 
	repeat, 
	get_char(S, C), 
	(   C == end_of_file ; write(C), fail ), !, 
	write('--------------------------'), nl, nl.



s  :-
	stream_property(X, alias(Y)), write(X-Y), nl, fail ; true.

aff :-
	current_stream(X), 
	write(X), nl, 
	fail.
aff.


closeall  :-
	current_stream(X), 
	close(X), 
	fail.

closeall.



test :-
	open('/tmp/foo', read, X, [alias(foo)]), write(X-foo), nl.

i :-
	exec('info', user_input, user_output, user_error).

i(S) :-
	exec('info', user_input, user_output, user_error, X), wait(X,S).

m :-
	exec(ls, null, user_output, null).
*/
/*
:- multifile(p/0).

p.
*/


% --- test
% display 1..N, followed by odd numbers 1,3,.. until largest odd N' <= N
s(N):- a(N), clause(aa(X),_), write(X),nl, X1 is X*2, retract(aa(X1)), fail.
s(_):- clause(aa(X), _), write(X), nl, fail.
s(_).


a(N):- between(1,N,I), assertz(aa(I)), fail.
a(_).


% --- test
% 10->56, 20->211, 30->466
t1(N, Z) :- f(N), findall(X,r1(X),L), length(L,Z).

f(N):- between(1,N,I), assertz(foo(_,I,I*I)), fail.
f(_).

% retract all without cut
r1([X-Y|L]) :- retract(foo(_,X,Y)), /* write(removed(X,Y)), nl, */ r1(L).
r1([]).

% retract all with a cut
r2([X-Y|L]) :- retract(foo(_,X,Y)), !, /*write(removed(X,Y)), nl*/ r2(L).
r2([]).


% --- test
% N->N
% with DEBUG_LEVEL = 1 and with MAX_CLAUSES_BEFORE_CLEAN set to 0 
% check unlink step by step (Delete followed by unlink+free)
t2(N, Z) :- f(N), r2(L), length(L, Z).


% --- test
% 5->5, then relaunch 5->15, 5->35, 5->75, 5->155 (2*prev+5)
t3(N, Z):- f(N), findall(X, u(X), L), length(L, Z).

u(X):- clause(foo(_,X,Y),_), write(foo(X,Y)), nl, assertz(foo(_,X,Y+Y)).


% --- test


z(X):- assertz(zz(1)), clause(zz(X),_), assertz(zz(X+z)).


:- dynamic(v/3).

v(1,1,1).
v(1,2,2).
v(2,3,3).
v(X,4,4+X).
v(4,5,5).
v(1,6,6).
v(X,7,7+X).
v(X,8,8+X).
v(4,9,9).

qv:- X=1, v(X,Y,Z), write(v(X,Y,Z)),nl,retract(v(1,V,W)),assertz(v(1,V+V,W+W)),fail.
qv.

:- dynamic(w/3).

w(1,1,1).
w(1,2,2).
w(1,3,4).
w(1,6,6).

qw:-X=1, clause(w(X,Y,Z),_), write(w(X,Y,Z)),nl,retract(w(X,V,W)),write(retract(X,V,W)),nl, V<3,assertz(w(1,V+V,W+W)), fail.
qw.

%:- initialization(qw).
:- endif.


d :- nth0(I,[A+B+C+D,1+2+3+4,A^B^C^D^E,1^2^3^4^nil,[1,2,3,4],[A,B,C,D],[- -A,- -B,- -C,- -D],[[A,B,C,D]]],T),
     between(0,5,M), write(I-M), write(' '),
     write_term(T,[variable_names(['A'=A,'B'=B,'C'=C,'D'=D,'E'=E]),max_depth(M)]), nl, false.

d :- absolute_file_name('../../../tmp/wt-depth-gp', F), tell(F),
     nth0(I,[A+B+C+D,1+2+3+4,A^B^C^D^E,1^2^3^4^nil,[1,2,3,4],[A,B,C,D],[- -A,- -B,- -C,- -D],[[A,B,C,D]]],T),
     between(0,5,M), write(I-M), write(' '),
     write_term(T,[variable_names(['A'=A,'B'=B,'C'=C,'D'=D,'E'=E]),max_depth(M)]), nl, false.

d :- told,
     write('-------------- diff ~/tmp/wt-depth-sics ~/tmp/wt-depth-gp -----------------'), nl,
     shell('diff --width=80 --suppress-common-lines --side-by-side ~/tmp/wt-depth-sics ~/tmp/wt-depth-gp').
	

