/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : assert.pl                                                       * 
 * Descr.: dynamic predicate management                                    * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2001 Daniel Diaz                                     * 
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
 * 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     * 
 *-------------------------------------------------------------------------*/

/* $Id$ */

:-	built_in.

'$use_assert'.


asserta(C) :-
	set_bip_name(asserta, 1),
	'$assert'(C, 1, 1).




assertz(C) :-
	set_bip_name(assertz, 1),
	'$assert'(C, 0, 1).




'$assert'(C, Asserta, CheckPerm) :-
	'$get_head_and_body'(C, H, B),
	'$term_to_goal'(B, none, B1),
	'$call_c'('Assert_4'(H, B1, Asserta, CheckPerm)),
	fail.

'$assert'(_, _, _).




retract(C) :-
	set_bip_name(retract, 1),
	'$get_head_and_body'(C, H, B),
	'$retract'(H, B).


'$retract'(H, B) :-      % call_c must be alone (inline) CP cannot be changed
	'$call_c_test'('Retract_2'(H, B)).




retractall(H) :-
	set_bip_name(retractall, 1),
	'$call_c_test'('Retractall_If_Empty_Head_1'(H)), !.

retractall(H) :-              % here only if Retractall_If_Empty_Head_1 fails
	'$retract'(H, _),
	fail.

retractall(_).




'$retract_last_found' :-
	'$call_c'('Retract_Last_Found_0').



clause(H, B) :-
	set_bip_name(clause, 2),
	'$check_head'(H),
	'$clause'(H, B, 0).


'$clause'(H, B, ForWhat) :-
                         % call_c must be alone (inline) CP cannot be changed
	'$call_c_test'('Clause_3'(H, B, ForWhat)).



'$instance_for_setarg'(H, B) :-
	'$call_c_test'('Clause_3'(H, B, 0)).

'$setarg_in_last_found'(ArgNo, NewValue) :-
	'$call_c'('Setarg_Of_Last_Found_2'(ArgNo, NewValue)).



abolish(PI) :-
	set_bip_name(abolish, 1),
	'$call_c'('Abolish_1'(PI)).




'$remove_predicate'(Name, Arity) :-
	'$call_c'('Remove_Predicate_2'(Name, Arity)).




'$scan_dyn_test_alt' :-             % used by C code to create a choice-point
	'$call_c_test'('Scan_Dynamic_Pred_Alt_0').

'$scan_dyn_jump_alt' :-             % used by C code to create a choice-point
	'$call_c_jump'('Scan_Dynamic_Pred_Alt_0').
