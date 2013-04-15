/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : call.pl                                                         *
 * Descr.: meta call management                                            *
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


forall(Condition, Action) :-
	'$not'((Condition, '$not'(Action, forall, 2)), forall, 2).


'$not'(Goal, Func, Arity) :-
	(   '$call'(Goal, Func, Arity, true) ->
	    fail
	;   true
	).
