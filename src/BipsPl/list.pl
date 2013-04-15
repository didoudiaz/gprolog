/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : list.pl                                                         *
 * Descr.: list library                                                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2013 Daniel Diaz                                     *
 *                                                                         *
 * This file is part of GNU Prolog                                         *
 *                                                                         *
 * GNU Prolog is free software: you can redistribute it and/or             *
 * modify it under the terms of either:                                    *
 *                                                                         *
 *   - the GNU Lesser General Public License as published by the Free      *
 *     Software Foundation; either version 3 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or                                                                      *
 *                                                                         *
 *   - the GNU General Public License as published by the Free             *
 *     Software Foundation; either version 2 of the License, or (at your   *
 *     option) any later version.                                          *
 *                                                                         *
 * or both in parallel, as here.                                           *
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful,           *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received copies of the GNU General Public License and   *
 * the GNU Lesser General Public License along with this program.  If      *
 * not, see http://www.gnu.org/licenses/.                                  *
 *-------------------------------------------------------------------------*/


:-	built_in.

'$use_list'.

/*
append([], L, L).

append([H|T1], List, [H|T2]) :-
	append(T1, List, T2).
*/

append(L1, L2, L3) :-
	'$call_c_test'('Pl_Append_3'(L1, L2, L3)).


'$append_alt' :-       % used by C code to create a choice-point
        '$call_c_test'('Pl_Append_Alt_0').




/*
member(X, [H|T]) :-
	(   X = H
	;   member(X, T)
	).
*/

member(_X, _L) :-
	'$call_c_test'('Pl_Member_2').

'$member_alt' :-       % used by C code to create a choice-point
        '$call_c_test'('Pl_Member_Alt_0').




/*
memberchk(X, L) :-
        (   X = H, !
        ;   memberchk(X, T)
        ).

*/

memberchk(X, L) :-
	'$call_c_test'('Pl_Memberchk_2'(X, L)).



/*
reverse([], []).

reverse([H|T], L) :-
	'$reverse1'(T, L, [H]).


'$reverse1'([], L, L).

'$reverse1'([H|T], L, L1) :-
	'$reverse1'(T, L, [H|L1]).
*/


reverse(L1, L2) :-
	'$call_c_test'('Pl_Reverse_2'(L1, L2)).

'$reverse_alt' :-       % used by C code to create a choice-point
        '$call_c_test'('Pl_Reverse_Alt_0').




delete([], _, []).

delete([H|T], X, L) :-
	H == X, !,
	delete(T, X, L).

delete([H|T], X, [H|L]) :-
	delete(T, X, L).





select(X, [X|T], T).

select(X, [H|T1], [H|T2]) :-
	select(X, T1, T2).





subtract([], _, []).

subtract([X|L1], L2, L3) :-
        memberchk(X, L2), !,
        subtract(L1, L2, L3).

subtract([X|L1], L2, [X|L3]) :-
        subtract(L1, L2, L3).
permutation([], []).

permutation(L, [H|T]) :-
	select(H, L, Rest),
	permutation(Rest, T).





prefix([], _).

prefix([X|T], [X|T1]) :-
	prefix(T, T1).




suffix(L, L).

suffix(X, [_|T]) :-
	suffix(X, T).




sublist(L, L).

sublist(Sub, [H|T]) :-
	'$sublist1'(T, H, Sub).


'$sublist1'(Sub, _, Sub).

'$sublist1'([H|T], _, Sub) :-
	'$sublist1'(T, H, Sub).

'$sublist1'([H|T], X, [X|Sub]) :-
	'$sublist1'(T, H, Sub).




last([H|T], X) :-
	'$last1'(T, H, X).

'$last1'([], X, X).

'$last1'([H|T], _, X) :-
	'$last1'(T, H, X).





/*
length(L, N) :-
        integer(N), !,
        N >= 0,
        '$make_list'(N, L).

length(L, N) :-
        '$length'(L, 0, N).


'$length'([], N, N).

'$length'([_|L], M, N) :-
        M1 is M + 1,
        '$length'(L, M1, N).

'$make_list'(0, []) :-
        !.

'$make_list'(N, [_|L]) :-
        N1 is N - 1,
        '$make_list'(N1, L).
*/



length(L, N) :-
	'$call_c_test'('Pl_Length_2'(L, N)).


'$length_alt' :-       % used by C code to create a choice-point
        '$call_c_test'('Pl_Length_Alt_0').





/*
nth(N, L, X) :-
	integer(N), !,
	N >= 1,
	'$nth1'(N, L, X).

nth(N, L, X) :-
	var(N),
	'$nth2'(L, X, 1, N).


'$nth1'(1, [X|_], X) :-
	!.

'$nth1'(N, [_|T], X) :-
	N1 is N - 1,
	'$nth1'(N1, T, X).


'$nth2'([X|_], X, N, N).

'$nth2'([_|T], X, I, N) :-
	I1 is I + 1,
	'$nth2'(T, X, I1, N).
*/


nth(N, L, X) :-
	nth1(N, L, X).

nth1(N, L, X) :-
	integer(N), !,
	'$call_c'('Pl_Nth0_3'(N, L, X, 1), [boolean, by_value]).

nth1(N, L, X) :-
	var(N),
	'$nth_gener'(L, X, 1, N).


nth0(N, L, X) :-
	integer(N), !,
	'$call_c'('Pl_Nth0_3'(N, L, X, 0), [boolean, by_value]).

nth0(N, L, X) :-
	var(N),
	'$nth_gener'(L, X, 0, N).



'$nth_gener'([X|_], X, N, N).

'$nth_gener'([_|L], X, I, N) :-
	I1 is I + 1,
	'$nth_gener'(L, X, I1, N).





max_list([H|T], Max) :-
	'$max_list1'(T, H, Max).

'$max_list1'([], Max, Max).

'$max_list1'([H|T], X, Max) :-
	H =< X, !,
	'$max_list1'(T, X, Max).

'$max_list1'([H|T], _, Max) :-
	'$max_list1'(T, H, Max).




min_list([H|T], Min) :-
	'$min_list1'(T, H, Min).

'$min_list1'([], Min, Min).

'$min_list1'([H|T], X, Min) :-
	H >= X, !,
	'$min_list1'(T, X, Min).

'$min_list1'([H|T], _, Min) :-
	'$min_list1'(T, H, Min).





sum_list(L, Sum) :-
	'$sum_list1'(L, 0, Sum).

'$sum_list1'([], Sum, Sum).

'$sum_list1'([H|T], Sum0, Sum) :-
	Sum1 is H + Sum0,
	'$sum_list1'(T, Sum1, Sum).



flatten(List, FlatList) :-
        '$flatten'(List, [], FlatList0), !,
        FlatList = FlatList0.

'$flatten'(Var, Tl, [Var|Tl]) :-
        var(Var), !.

'$flatten'([], Tl, Tl) :- !.

'$flatten'([Hd|Tl], Tail, List) :- !,
        '$flatten'(Hd, FlatHeadTail, List),
        '$flatten'(Tl, Tail, FlatHeadTail).

'$flatten'(NonList, Tl, [NonList|Tl]).





maplist(Goal, List) :-
        '$maplist'(List, Goal).

'$maplist'([], _).

'$maplist'([X|List], Goal) :-
        call(Goal, X),
        '$maplist'(List, Goal).




maplist(Goal, L1, L2) :-
        '$maplist'(L1, L2, Goal).

'$maplist'([], [], _).

'$maplist'([X1|L1], [X2|L2], Goal) :-
        call(Goal, X1, X2),
        '$maplist'(L1, L2, Goal).




maplist(Goal, L1, L2, L3) :-
        '$maplist'(L1, L2, L3, Goal).

'$maplist'([], [], [], _).

'$maplist'([X1|L1], [X2|L2], [X3|L3], Goal) :-
        call(Goal, X1, X2, X3),
        '$maplist'(L1, L2, L3, Goal).




maplist(Goal, L1, L2, L3, L4) :-
        '$maplist'(L1, L2, L3, L4, Goal).

'$maplist'([], [], [], [], _).

'$maplist'([X1|L1], [X2|L2], [X3|L3], [X4|L4], Goal) :-
        call(Goal, X1, X2, X3, X4),
        '$maplist'(L1, L2, L3, L4, Goal).




maplist(Goal, L1, L2, L3, L4, L5) :-
        '$maplist'(L1, L2, L3, L4, L5, Goal).

'$maplist'([], [], [], [], [], _).

'$maplist'([X1|L1], [X2|L2], [X3|L3], [X4|L4], [X5|L5], Goal) :-
        call(Goal, X1, X2, X3, X4, X5),
        '$maplist'(L1, L2, L3, L4, L5, Goal).




maplist(Goal, L1, L2, L3, L4, L5, L6) :-
        '$maplist'(L1, L2, L3, L4, L5, L6, Goal).

'$maplist'([], [], [], [], [], [], _).

'$maplist'([X1|L1], [X2|L2], [X3|L3], [X4|L4], [X5|L5], [X6|L6], Goal) :-
        call(Goal, X1, X2, X3, X4, X5, X6),
        '$maplist'(L1, L2, L3, L4, L5, L6, Goal).




maplist(Goal, L1, L2, L3, L4, L5, L6, L7) :-
        '$maplist'(L1, L2, L3, L4, L5, L6, L7, Goal).

'$maplist'([], [], [], [], [], [], [], _).

'$maplist'([X1|L1], [X2|L2], [X3|L3], [X4|L4], [X5|L5], [X6|L6], [X7|L7], Goal) :-
        call(Goal, X1, X2, X3, X4, X5, X6, X7),
        '$maplist'(L1, L2, L3, L4, L5, L6, L7, Goal).




maplist(Goal, L1, L2, L3, L4, L5, L6, L7, L8) :-
        '$maplist'(L1, L2, L3, L4, L5, L6, L7, L8, Goal).

'$maplist'([], [], [], [], [], [], [], [], _).

'$maplist'([X1|L1], [X2|L2], [X3|L3], [X4|L4], [X5|L5], [X6|L6], [X7|L7], [X8|L8], Goal) :-
        call(Goal, X1, X2, X3, X4, X5, X6, X7, X8),
        '$maplist'(L1, L2, L3, L4, L5, L6, L7, L8, Goal).

