/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : utils.pl                                                        *
 * Descr.: utilities                                                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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

'$get_head_and_body'(Clause, Module, Head, Body) :-
	'$call_c_test'('Pl_Get_Head_And_Body_4'(Clause, Module, Head, Body)).

'$term_to_goal'(P, Module, CallInfo, P1) :-
	'$call_c_test'('Pl_Term_To_Goal_4'(P, Module, CallInfo, P1)).

		      
/*
'$term_to_goal'(P, Module, CallInfo, P1) :-
	g_assign('$term_to_goal_module', Module),
	g_assign('$term_to_goal_info', CallInfo),
	'$term_to_goal1'(P, P1) , !.

'$term_to_goal'(P, _, _, _) :-
	'$pl_err_type'(callable, P).


'$term_to_goal1'(P, P1) :-
	var(P), !,
	g_read('$term_to_goal_call_info', CallInfo),
	g_read('$term_to_goal_module', Module),
	(   integer(CallInfo) ->		% for call
	    P1 = '$call_internal'(P, Module, CallInfo)
	;
	    atom(Module) -> 
	    P1 = call(Module:P)
	;
	    P1 = call(P)
	).

'$term_to_goal1'(Module:P, P1) :-
	!,
	g_read('$term_to_goal_module', SaveModule),
	g_assign('$term_to_goal_module', Module),
	'$term_to_goal1'(P, P1),
	g_assign('$term_to_goal_module', SaveModule).

'$term_to_goal1'((P -> Q), (P1 -> Q1)) :-
	!,
	'$term_to_goal1'(P, P1),
	'$term_to_goal1'(Q, Q1).

'$term_to_goal1'((P *-> Q), (P1 *-> Q1)) :-
	!,
	'$term_to_goal1'(P, P1),
	'$term_to_goal1'(Q, Q1).

'$term_to_goal1'((P, Q), (P1, Q1)) :-
	!,
	'$term_to_goal1'(P, P1),
	'$term_to_goal1'(Q, Q1).

'$term_to_goal1'((P ; Q), (P1 ; Q1)) :-
	!,
	'$term_to_goal1'(P, P1),
	'$term_to_goal1'(Q, Q1).
	
'$term_to_goal1'(P,  P) :-
	P = call(_), !.

'$term_to_goal1'(P, P) :-
	P = catch(_, _, _), !.

'$term_to_goal1'(P, P1) :-
	callable(P),
	g_read('$term_to_goal_module', Module),
	(   atom(Module) -> 
	    P1 = Module:P
	;
	    P1 = P
	).

*/




'$check_list'(List) :-
	list(List), !.

'$check_list'(List) :-
	list_or_partial_list(List), !,
	'$pl_err_instantiation'.

'$check_list'(List) :-
	'$pl_err_type'(list, List).




'$check_list_or_partial_list'(List) :-
	list_or_partial_list(List), !.

'$check_list_or_partial_list'(List) :-
	'$pl_err_type'(list, List).




'$check_atom_or_atom_list'(List) :-
	atom(List), !.

'$check_atom_or_atom_list'(List) :-
	'$check_atom_or_atom_list1'(List), !.


'$check_atom_or_atom_list1'(List) :-
	var(List),
	'$pl_err_instantiation'.

'$check_atom_or_atom_list1'([]).

'$check_atom_or_atom_list1'([X|List]) :-
	'$check_atom_or_atom_list2'(X),
	'$check_atom_or_atom_list1'(List).

'$check_atom_or_atom_list1'(List) :-
	'$pl_err_type'(list, List).


'$check_atom_or_atom_list2'(MX) :-
	'$strip_module_nonvar'(MX, _, X), % NB: also accepts module qualification
	atom(X), !.
/*
'$check_atom_or_atom_list2'(X) :-
	var(X), !,
	'$pl_err_instantiation'. %detected by '$strip_module_nonvar'
*/
'$check_atom_or_atom_list2'(X) :-
	'$pl_err_type'(atom, X).








'$check_head'(H) :-
	var(H), !,
	'$pl_err_instantiation'.

'$check_head'(H) :-
	(   callable(H) ->
	    true
	;   '$pl_err_type'(callable, H)
	).




'$check_nonvar'(X) :-
	nonvar(X), !.

'$check_nonvar'(_) :-
	'$pl_err_instantiation'.






'$get_pred_indic'(PI, DefModule, Module, Func, Arity) :-
	'$call_c_test'('Pl_Get_Pred_Indic_5'(PI, DefModule, Module, Func, Arity)).


'$get_pred_indic_var'(PI, DefModule, Module, Func, Arity) :-
	'$call_c_test'('Pl_Get_Pred_Indic_Var_5'(PI, DefModule, Module, Func, Arity)).
