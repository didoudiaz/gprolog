/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : call.pl                                                         *
 * Descr.: meta call management                                            *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2025 Daniel Diaz                                     *
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

'$use_call'.


:- meta_predicate(once(0)).

once(Goal) :-
	call(Goal), !.


:- meta_predicate(\+(0)).

\+ Goal :-
	(   call(Goal) ->
	    fail
	;   true
	).

:- meta_predicate(call_det(0, ?)).

call_det(Goal, Deterministic) :-
	set_bip_name(call_det, 2),
	(   nonvar(Deterministic),
	    Deterministic \== false,
	    Deterministic \== true ->
	    '$pl_err_type'(boolean, Deterministic)
	;   true
	),
	'$call_det'(Goal, Deterministic).
	
'$call_det'(Goal, Deterministic) :-
	'$get_current_B'(B),
	call(Goal),
	'$get_current_B'(B1),
	(   B1 > B ->
	    Deterministic = false
	;   Deterministic = true
	).




:- meta_predicate(call_nth(0, ?)).

call_nth(Goal, Nth) :-
	var(Nth), !,		% most common case first
	'$call_nth_exec'(Goal, Nth).

call_nth(Goal, Nth) :-
	integer(Nth), !,
	'$call_nth_int'(Goal, Nth).

call_nth(_, Nth) :-
	set_bip_name(call_nth, 2),
	'$pl_err_type'(integer, Nth).


'$call_nth_int'(Goal, Nth) :-
	Nth > 0, !,			% cut for if-then-else
	'$call_nth_exec'(Goal, Nth), !.	% cut since execute only once

'$call_nth_int'(_, Nth) :-	% simply fail if Nth = 0
	Nth < 0, !,
	set_bip_name(call_nth, 2),
	'$pl_err_domain'(not_less_than_zero, Nth).

/* version storing the counter in a term and modifying across
 * backtracking with nb_setarg
 *
'$call_nth_exec'(Goal, Nth) :-
	CounterTerm = '$call_nth_counter'(0, _), % see issue #45 (for future representation sharing)
	'$call'(Goal, call_nth, 2, true),
	arg(1, CounterTerm, Counter),
	Counter1 is Counter + 1,
	setarg(1, CounterTerm, Counter1, false),
	Nth = Counter1.		% must be after counter increment
 */

/* Optimized version storing the counter in a WAM permanent Y variable
 * (see issue #45). For this, use the (new) option to '$call_c': ret(RetVar).
 * RetVar must be "local" variable (i.e. a Y var stored in the environment).
 * The '$call_c' performs a C-like assignment: Counter += 1
 * To test int_overflow, init with:
 *	Counter = 1152921504606846974, (max_integer-1) to have 1 success
 */

'$call_nth_exec'(Goal, Nth) :-
	Counter = 0,		% Counter stored in a WAM Y variable
	'$call'(Goal, call_nth, 2, true),
	'$call_c'('Pl_Fct_Inc'(Counter), [fast_call, ret(Counter)]),
	Nth = Counter.		% do not pass Nth directly above !




/* Optimized version storing the counter in a WAM permanent Y variable
 * (see issue #45). For this, use the (new) option to '$call_c': ret(RetVar).
 * RetVar must be "local" variable (i.e. a Y var stored in the environment).
 * The '$call_c' performs a C-like assignment: Counter += 1
 * NB: the counter must remain in a same Y variable. Must not be incremented 
 * in an auxiliary predicate (so not in a if-then-else construct).
 * For this we use an exec_goal which always succeeds, increment counter and
 * then test last case, so succeeds at least once (init counter with -1, this
 * is important to detect int_overflow.
 * To test int_overflow, init with:
 *	Counter = 1152921504606846973, (max_integer-2) to have 1 success
 */

:- meta_predicate(countall(0, ?)).

countall(Goal, N) :-
	set_bip_name(countall, 2),
	'$call_c'('Pl_Check_For_Un_Positive'(N)),
	Counter = -1,		% Counter stored in a WAM Y variable
	'$countall_exec_goal'(Goal, Stop),
	'$call_c'('Pl_Fct_Inc'(Counter), [fast_call, ret(Counter)]),
	( var(Stop) ->
	    fail
	;   N = Counter
	).

'$countall_exec_goal'(Goal, _Stop) :-
	'$call'(Goal, countall, 2, true).

'$countall_exec_goal'(_Goal, stop).




	% the internal call predicate (also called by pl2wam)

'$call'(Goal, Func, Arity, DebugCall) :-
	'$call_c'('Pl_Save_Call_Info_3'(Func, Arity, DebugCall)),
	'$call1'(Goal, 0).

'$call1'(Goal, CallInfo) :-
	'$call_c'('Pl_Load_Call_Info_Arg_1'(1)),   % to ensure CallInfo is deref
	'$call_internal'(Goal, CallInfo).

'$call_internal'(Goal, CallInfo) :-
	'$call_c'('Pl_Call_Info_Bip_Name_1'(CallInfo)),
	(   var(Goal) ->
	    '$pl_err_instantiation'
	;   true
	),
	'$term_to_goal'(Goal, CallInfo, Goal1),
	'$call_internal1'(Goal1, CallInfo).


'$call_internal1'(Goal, CallInfo) :-
	'$get_cut_level'(VarCut),         % must be the first goal (A(2)=cut)
	'$call_internal_with_cut'(Goal, CallInfo, VarCut).


                                    % also called by C code BC_Emulate_Clause

'$call_internal_with_cut'((P, Q), CallInfo, VarCut) :-
	!,
	'$call_internal_with_cut'(P, CallInfo, VarCut),
	'$call_internal_with_cut'(Q, CallInfo, VarCut).

'$call_internal_with_cut'((P ; Q), CallInfo, VarCut) :-
	!,
	'$call_internal_or'(P, Q, CallInfo, VarCut).

'$call_internal_with_cut'(!, _CallInfo, VarCut) :-
% !,                               this cut is useless because '$cut'/1
	'$cut'(VarCut).

'$call_internal_with_cut'((P -> Q), CallInfo, VarCut) :-
	!,
	'$call_internal'(P, CallInfo), !,
	'$call_internal_with_cut'(Q, CallInfo, VarCut).

	% P *-> Q alone (i.e. not inside a ;) is logically the same as P, Q. 
        % However a cut in the test part (P) should be local to P (as in P -> Q).
'$call_internal_with_cut'((P *-> Q), CallInfo, VarCut) :-
	!,
	'$call_internal'(P, CallInfo),
	'$call_internal_with_cut'(Q, CallInfo, VarCut).

'$call_internal_with_cut'(fail, _CallInfo, _VarCut) :-
	!,
	fail.

'$call_internal_with_cut'(true, _CallInfo, _VarCut) :-
	!.

'$call_internal_with_cut'(call(Goal), CallInfo, _VarCut) :-
	!,
	'$call_internal'(Goal, CallInfo).

'$call_internal_with_cut'(catch(Goal, Catch, Recovery), CallInfo, _VarCut) :-
	!,
	'$catch_internal'(Goal, Catch, Recovery, CallInfo).

'$call_internal_with_cut'(throw(Ball), CallInfo, _VarCut) :-
	!,
	'$throw_internal'(Ball, CallInfo).

'$call_internal_with_cut'(P, CallInfo, _VarCut) :-
	'$call_c_jump'('Pl_BC_Call_Terminal_Pred_3'(P, CallInfo, 1)).




'$call_internal_or'((P -> Q), R, CallInfo, VarCut) :-
	!,
	(   '$call_internal'(P, CallInfo), !,
	    '$call_internal_with_cut'(Q, CallInfo, VarCut)
	;   '$call_internal_with_cut'(R, CallInfo, VarCut)
	).

'$call_internal_or'((P *-> Q), R, CallInfo, VarCut) :-
	!,
	(   '$call_internal'(P, CallInfo) *->
	    '$call_internal_with_cut'(Q, CallInfo, VarCut)
	;   '$call_internal_with_cut'(R, CallInfo, VarCut)
	).

'$call_internal_or'(P, _, CallInfo, VarCut) :-
	'$call_internal_with_cut'(P, CallInfo, VarCut).

'$call_internal_or'(_, Q, CallInfo, VarCut) :-
	'$call_internal_with_cut'(Q, CallInfo, VarCut).




'$call_from_debugger'(Goal, CallInfo) :-
	'$call_c_jump'('Pl_BC_Call_Terminal_Pred_3'(Goal, CallInfo, 0)).



false :-
	fail.


:- meta_predicate(forall(0, 0)).

forall(Condition, Action) :-
	'$not'((Condition, '$not'(Action, forall, 2)), forall, 2).


'$not'(Goal, Func, Arity) :-
	(   '$call'(Goal, Func, Arity, true) ->
	    fail
	;   true
	).
