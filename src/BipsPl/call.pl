/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : call.pl                                                         *
 * Descr.: meta call management                                            *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2021 Daniel Diaz                                     *
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




/* Goal the goal to execute
 * Module is the lookup module for the Goal (i.e. Module:Goal)
 * (this is passed separately to avoid meta-arg creation)
 * CallerFunc/CallerArity: the pred/arity which invoked the call (head of the clause)
 * NB: add the CallerModule (as CallerMFA: Module/Func/Arity) also for catch/throw
 * for instance in module foo:p(X):-  bar:call(X), the CallerMFA is foo:p/1 but Module is bar.
 *
 * To be fast a C function is first called: it mainly handles a call to a predicate
 * (and some simple control constructs).
 *
 * For more complex constructs it calls (via return) '$call_internal_xxx' predicates
 * It has set bip_name to CallerFunc/CallerArity.
 * The predicates containing '_internal' accept a CallInfo (which is dereferenced)
 */


'$call'(Goal, Module, CallerFunc, CallerArity) :-
	'$call_c'('Pl_BC_Call_5'(Goal, Module, CallerFunc, CallerArity, 1), [jump, by_value]).


'$call_no_debug'(Goal, Module, CallerFunc, CallerArity) :-
	'$call_c'('Pl_BC_Call_5'(Goal, Module, CallerFunc, CallerArity, 0), [jump, by_value]).


	/* called by Pl_BC_Call_5 for control-constructs , ; -> *-> (all of arity = 2) */

'$call_internal_and'(P, Q, Module, CallInfo, VarCut) :-
	'$call_internal_with_cut'(P, Module, CallInfo, VarCut),
	'$call_internal_with_cut'(Q, Module, CallInfo, VarCut).
	
'$call_internal_if'(P, Q, Module, CallInfo, VarCut) :-
	!,
	'$call_internal'(P, Module, CallInfo), !,
	'$call_internal_with_cut'(Q, Module, CallInfo, VarCut).

	% P *-> Q alone (i.e. not inside a ;) is logically the same as P, Q. 
        % However a cut in the test part (P) should be local to P (as in P -> Q).
'$call_internal_soft_if'(P, Q, Module, CallInfo, VarCut) :-
	!,
	'$call_internal'(P, Module, CallInfo),
	'$call_internal_with_cut'(Q, Module, CallInfo, VarCut).


 	/* call_internal / call_internal_with_cut */

'$call_internal'(Goal, Module, CallInfo) :-
	'$get_cut_level'(VarCut),         % must be the first goal (A(arity)=cut)
	'$call_internal_with_cut'(Goal, Module, CallInfo, VarCut).


                                    % also called by C code BC_Emulate_Clause



'$call_internal_with_cut'(Goal, _, CallInfo, _) :-
	var(Goal), !,
	'$call_c'('Pl_Call_Info_Bip_Name_1'(CallInfo)),
	'$pl_err_instantiation'.

'$call_internal_with_cut'(Module:P, _, CallInfo, VarCut) :-
	!,
	'$call_internal_with_cut'(P, Module, CallInfo, VarCut).

'$call_internal_with_cut'((P, Q), Module, CallInfo, VarCut) :-
	!,
	'$call_internal_with_cut'(P, Module, CallInfo, VarCut),
	'$call_internal_with_cut'(Q, Module, CallInfo, VarCut).

'$call_internal_with_cut'((P ; Q), Module, CallInfo, VarCut) :-
	!,
	'$call_internal_or'(P, Q, Module, CallInfo, VarCut).

'$call_internal_with_cut'(!, _Module, _CallInfo, VarCut) :-
% !,                               this cut is useless because '$cut'/1
	'$cut'(VarCut).

'$call_internal_with_cut'((P -> Q), Module, CallInfo, VarCut) :-
	!,
	'$call_internal'(P, Module, CallInfo), !,
	'$call_internal_with_cut'(Q, Module, CallInfo, VarCut).

	% P *-> Q alone (i.e. not inside a ;) is logically the same as P, Q. 
        % However a cut in the test part (P) should be local to P (as in P -> Q).
'$call_internal_with_cut'((P *-> Q), Module, CallInfo, VarCut) :-
	!,
	'$call_internal'(P, Module, CallInfo),
	'$call_internal_with_cut'(Q, Module, CallInfo, VarCut).

'$call_internal_with_cut'(fail, _Module, _CallInfo, _VarCut) :-
	!,
	fail.

'$call_internal_with_cut'(true, _Module, _CallInfo, _VarCut) :-
	!.

'$call_internal_with_cut'(call(Goal), Module, CallInfo, _VarCut) :-
	!,
	'$call_internal'(Goal, Module, CallInfo).

'$call_internal_with_cut'(catch(Goal, Catch, Recovery), Module, CallInfo, _VarCut) :-
	!,
	'$catch_internal'(Goal, Catch, Recovery, Module, CallInfo).

'$call_internal_with_cut'(throw(Ball), Module, CallInfo, _VarCut) :-
	!,
	'$throw_internal'(Ball, Module, CallInfo).   % FIXME what to do with Module in throw (for error report, this is not the good Module (should be CallerModule))

'$call_internal_with_cut'(P, Module, CallInfo, _VarCut) :-
	'$call_c_jump'('Pl_BC_Call_Terminal_Pred_4'(P, Module, CallInfo, 1)).




'$call_internal_or'((P -> Q), R, Module, CallInfo, VarCut) :-
	!,
	(   '$call_internal'(P, Module, CallInfo), !,
	    '$call_internal_with_cut'(Q, Module, CallInfo, VarCut)
	;   '$call_internal_with_cut'(R, Module, CallInfo, VarCut)
	).

'$call_internal_or'((P *-> Q), R, Module, CallInfo, VarCut) :-
	!,
	(   '$call_internal'(P, Module, CallInfo) *->
	    '$call_internal_with_cut'(Q, Module, CallInfo, VarCut)
	;   '$call_internal_with_cut'(R, Module, CallInfo, VarCut)
	).

'$call_internal_or'(P, _, Module, CallInfo, VarCut) :-
	'$call_internal_with_cut'(P, Module, CallInfo, VarCut).

'$call_internal_or'(_, Q, Module, CallInfo, VarCut) :-
	'$call_internal_with_cut'(Q, Module, CallInfo, VarCut).




'$call_from_debugger'(Goal, Module, CallInfo) :-
	'$call_c_jump'('Pl_BC_Call_Terminal_Pred_4'(Goal, Module, CallInfo, 0)).



false :-
	fail.


:- meta_predicate(forall(0, 0)).

forall(Condition, Action) :-
	'$not'((Condition, '$not'(Action, forall, 2)), forall, 2).


'$not'(Goal, Func, Arity) :-
	(   '$call'(Goal, user, Func, Arity) -> % user is OK since Goal is qualified (meta_predicate)
	    fail
	;   true
	).
