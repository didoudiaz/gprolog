/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : call.pl                                                         *
 * Descr.: meta call management                                            *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2012 Daniel Diaz                                     *
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

/* $Id$ */

:-	built_in.

'$use_call'.



once(Goal) :-
	call(Goal), !.


\+ Goal :-
	(   call(Goal) ->
	    fail
	;   true
	).



call_det(Goal, Deterministic) :-
	(   nonvar(Deterministic),
	    Deterministic \== false,
	    Deterministic \== true ->
	    set_bip_name(call, 2),
	    '$pl_err_type'(boolean, Deterministic)
	;   true
	),
	'$get_current_B'(B),
	call(Goal),
	'$get_current_B'(B1),
	(   B1 > B ->
	    Deterministic = false
	;   Deterministic = true
	).



/* Goal the goal to execute
 * CallerModule is the module where the call comes from (calling module)
 * CallerFunc/CallerArity: the pred/arity which invoked the call (head of the clause)
 *     NB: Goal can be of the form '$meta_term'(Offset) providing another CallerModule and Goal
 *
 * To be fast a C function is first called: it mainly handles a call to a predicate
 * (and some simple control constructs).
 *
 * For more complex constructs it calls (via return) '$call_complex'
 * It has set bip_name to CallerFunc/CallerArity.
 * CallInfo is dereferenced
 * QualifModule the module qualification of a goal (default: same as CallerModule)
 */

'$call'(Goal, CallerModule, CallerFunc, CallerArity, DebugCall) :-
	'$call_c_jump'('Pl_BC_Call_Initial_5'(Goal, CallerModule, CallerFunc, CallerArity, DebugCall)).


				% here we know Goal is not a variable (checked by Pl_BC_Call_Initial_4) 
'$call_complex'(Goal, CallerModule, QualifModule, CallInfo) :-
 	'$term_to_goal'(Goal, CallInfo, CallerModule, QualifModule, Goal1),
	'$call_internal'(Goal1, CallerModule, CallInfo).
 	

'$call_internal'(Goal, CallerModule, CallInfo) :-
	'$get_cut_level'(VarCut),         % must be the first goal (A(arity)=cut)
	'$call_internal_with_cut'(Goal, CallerModule, CallInfo, VarCut).


                                    % also called by C code BC_Emulate_Clause

'$call_internal_with_cut'((P, Q), CallerModule, CallInfo, VarCut) :-
	!,
	'$call_internal_with_cut'(P, CallerModule, CallInfo, VarCut),
	'$call_internal_with_cut'(Q, CallerModule, CallInfo, VarCut).

'$call_internal_with_cut'((P ; Q), CallerModule, CallInfo, VarCut) :-
	!,
	'$call_internal_or'(P, Q, CallerModule, CallInfo, VarCut).

'$call_internal_with_cut'(!, _CallerModule, _CallInfo, VarCut) :-
% !,                               this cut is useless because '$cut'/1
	'$cut'(VarCut).

'$call_internal_with_cut'((P -> Q), CallerModule, CallInfo, VarCut) :-
	!,
	'$call_internal'(P, CallerModule, CallInfo), !,
	'$call_internal_with_cut'(Q, CallerModule, CallInfo, VarCut).

	% P *-> Q alone (i.e. not inside a ;) is logically the same as P, Q. 
        % However a cut in the test part (P) should be local to P (as in P -> Q).
'$call_internal_with_cut'((P *-> Q), CallerModule, CallInfo, VarCut) :-
	!,
	'$call_internal'(P, CallerModule, CallInfo),
	'$call_internal_with_cut'(Q, CallerModule, CallInfo, VarCut).

'$call_internal_with_cut'(fail, _CallerModule, _CallInfo, _VarCut) :-
	!,
	fail.

'$call_internal_with_cut'(true, _CallerModule, _CallInfo, _VarCut) :-
	!.

'$call_internal_with_cut'(call(Goal), CallerModule, CallInfo, _VarCut) :-
	!,
	'$call_internal'(Goal, CallerModule, CallInfo).

'$call_internal_with_cut'(catch(Goal, Catch, Recovery), CallerModule, CallInfo, _VarCut) :-
	!,
	'$catch_internal'(Goal, Catch, Recovery, CallerModule, CallInfo).

'$call_internal_with_cut'(throw(Ball), CallerModule, CallInfo, _VarCut) :-
	!,
	'$throw_internal'(Ball, CallerModule, CallInfo).

'$call_internal_with_cut'(P, CallerModule, CallInfo, _VarCut) :-
	'$call_c_jump'('Pl_BC_Call_Terminal_Pred_4'(P, CallerModule, CallInfo, 1)).




'$call_internal_or'((P -> Q), R, CallerModule, CallInfo, VarCut) :-
	!,
	(   '$call_internal'(P, CallerModule, CallInfo), !,
	    '$call_internal_with_cut'(Q, CallerModule, CallInfo, VarCut)
	;   '$call_internal_with_cut'(R, CallerModule, CallInfo, VarCut)
	).

'$call_internal_or'((P *-> Q), R, CallerModule, CallInfo, VarCut) :-
	!,
	(   '$call_internal'(P, CallerModule, CallInfo) *->
	    '$call_internal_with_cut'(Q, CallerModule, CallInfo, VarCut)
	;   '$call_internal_with_cut'(R, CallerModule, CallInfo, VarCut)
	).

'$call_internal_or'(P, _, CallerModule, CallInfo, VarCut) :-
	'$call_internal_with_cut'(P, CallerModule, CallInfo, VarCut).

'$call_internal_or'(_, Q, CallerModule, CallInfo, VarCut) :-
	'$call_internal_with_cut'(Q, CallerModule, CallInfo, VarCut).




'$call_from_debugger'(Goal, CallerModule, CallInfo) :-
	'$call_c_jump'('Pl_BC_Call_Terminal_Pred_4'(Goal, CallerModule, CallInfo, 0)).



false :-
	fail.


forall(Condition, Action) :-
	'$not'((Condition, '$not'(Action, forall, 2)), forall, 2).


'$not'(Goal, Func, Arity) :-
	(   '$call'(Goal, user, Func, Arity, true) -> % FIXME CallerModule
	    fail
	;   true
	).
