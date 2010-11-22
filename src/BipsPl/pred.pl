/*-------------------------------------------------------------------------* 
 * GNU Prolog                                                              * 
 *                                                                         * 
 * Part  : Prolog buit-in predicates                                       * 
 * File  : pred.pl                                                         * 
 * Descr.: predicate manipulation management                               * 
 * Author: Daniel Diaz                                                     * 
 *                                                                         * 
 * Copyright (C) 1999-2010 Daniel Diaz                                     * 
 *                                                                         * 
 * GNU Prolog is free software; you can redistribute it and/or modify it   * 
 * under the terms of the GNU Lesser General Public License as published   * 
 * by the Free Software Foundation; either version 3, or any later version.* 
 *                                                                         * 
 * GNU Prolog is distributed in the hope that it will be useful, but       * 
 * WITHOUT ANY WARRANTY; without even the implied warranty of              * 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        * 
 * General Public License for more details.                                * 
 *                                                                         * 
 * You should have received a copy of the GNU Lesser General Public License* 
 * with this program; if not, write to the Free Software Foundation, Inc.  * 
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               * 
 *-------------------------------------------------------------------------*/

/* $Id$ */

:-	built_in.

'$use_pred'.


current_predicate(PI) :-
	set_bip_name(current_predicate, 1),
	'$current_predicate'(PI).

'$current_predicate'(PI) :-
	'$call_c_test'('Pl_Current_Predicate_2'(PI, 0)).


'$current_predicate_bips'(PI) :-
	'$call_c_test'('Pl_Current_Predicate_2'(PI, 1)).


'$current_predicate_any'(PI) :-
	'$call_c_test'('Pl_Current_Predicate_2'(PI, 2)).


'$current_predicate_alt' :-         % used by C code to create a choice-point
	'$call_c_test'('Pl_Current_Predicate_Alt_0').




predicate_property(PI, Property) :-
	set_bip_name(predicate_property, 2),
	'$current_predicate_bips'(PI),
	'$predicate_property1'(PI, Property).


'$predicate_property_any'(PI, Property) :-
	'$current_predicate_any'(PI),
	'$predicate_property1'(PI, Property).




'$predicate_property1'(PI, Property) :-
	'$check_pred_prop'(Property), !,
	'$predicate_property2'(Property, PI).




'$check_pred_prop'(Property) :-
	var(Property).

'$check_pred_prop'(static).

'$check_pred_prop'(dynamic).

'$check_pred_prop'(private).

'$check_pred_prop'(public).

'$check_pred_prop'(monofile).

'$check_pred_prop'(multifile).

'$check_pred_prop'(user).

'$check_pred_prop'(built_in).

'$check_pred_prop'(built_in_fd).

'$check_pred_prop'(native_code).

'$check_pred_prop'(prolog_file(_)).

'$check_pred_prop'(prolog_line(_)).

'$check_pred_prop'(meta_predicate(_)).

'$check_pred_prop'(Property) :-
	'$pl_err_domain'(predicate_property, Property).




'$predicate_property2'(static, PI) :-
	'$call_c_test'('Pl_Pred_Prop_Static_1'(PI)).

'$predicate_property2'(dynamic, PI) :-
	'$call_c_test'('Pl_Pred_Prop_Dynamic_1'(PI)).

'$predicate_property2'(private, PI) :-
	'$call_c_test'('Pl_Pred_Prop_Private_1'(PI)).

'$predicate_property2'(public, PI) :-
	'$call_c_test'('Pl_Pred_Prop_Public_1'(PI)).

'$predicate_property2'(monofile, PI) :-
	'$call_c_test'('Pl_Pred_Prop_Monofile_1'(PI)).

'$predicate_property2'(multifile, PI) :-
	'$call_c_test'('Pl_Pred_Prop_Multifile_1'(PI)).

'$predicate_property2'(user, PI) :-
	'$call_c_test'('Pl_Pred_Prop_User_1'(PI)).

'$predicate_property2'(built_in, PI) :-
	'$call_c_test'('Pl_Pred_Prop_Built_In_1'(PI)).

'$predicate_property2'(built_in_fd, PI) :-
	'$call_c_test'('Pl_Pred_Prop_Built_In_Fd_1'(PI)).

'$predicate_property2'(native_code, PI) :-
	'$call_c_test'('Pl_Pred_Prop_Native_Code_1'(PI)).

'$predicate_property2'(prolog_file(PlFile), PI) :-
	'$call_c_test'('Pl_Pred_Prop_Prolog_File_2'(PI, PlFile)).

'$predicate_property2'(prolog_line(PlLine), PI) :-
	'$call_c_test'('Pl_Pred_Prop_Prolog_Line_2'(PI, PlLine)).

'$predicate_property2'(meta_predicate(MetaPredTerm), PI) :-
	'$get_pred_indicator'(PI, Func, Arity),
	'$prop_meta_pred'(Func, Arity, MetaPredTerm).



'$prop_meta_pred'(\+, 1, \+(0)).
'$prop_meta_pred'(abolish, 1, abolish(:)).
'$prop_meta_pred'(asserta, 1, asserta(:)).
'$prop_meta_pred'(assertz, 1, assertz(:)).
'$prop_meta_pred'(bagof, 3, bagof(?, 0, -)).
'$prop_meta_pred'(call, 2, call(1, ?)).
'$prop_meta_pred'(call, 3, call(2, ?, ?)).
'$prop_meta_pred'(call, 4, call(3, ?, ?, ?)).
'$prop_meta_pred'(call, 5, call(4, ?, ?, ?, ?)).
'$prop_meta_pred'(call, 6, call(5, ?, ?, ?, ?, ?)).
'$prop_meta_pred'(call, 7, call(6, ?, ?, ?, ?, ?, ?)).
'$prop_meta_pred'(call, 8, call(7, ?, ?, ?, ?, ?, ?, ?)).
'$prop_meta_pred'(call, 9, call(8, ?, ?, ?, ?, ?, ?, ?, ?)).
'$prop_meta_pred'(call, 10, call(9, ?, ?, ?, ?, ?, ?, ?, ?, ?)).
'$prop_meta_pred'(call, 11, call(10, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)).
'$prop_meta_pred'(call_det, 2, call_det(0, ?)).
'$prop_meta_pred'(call_with_args, 1, call_with_args(1)).
'$prop_meta_pred'(call_with_args, 2, call_with_args(1, ?)).
'$prop_meta_pred'(call_with_args, 3, call_with_args(2, ?, ?)).
'$prop_meta_pred'(call_with_args, 4, call_with_args(3, ?, ?, ?)).
'$prop_meta_pred'(call_with_args, 5, call_with_args(4, ?, ?, ?, ?)).
'$prop_meta_pred'(call_with_args, 6, call_with_args(5, ?, ?, ?, ?, ?)).
'$prop_meta_pred'(call_with_args, 7, call_with_args(6, ?, ?, ?, ?, ?, ?)).
'$prop_meta_pred'(call_with_args, 8, call_with_args(7, ?, ?, ?, ?, ?, ?, ?)).
'$prop_meta_pred'(call_with_args, 9, call_with_args(8, ?, ?, ?, ?, ?, ?, ?, ?)).
'$prop_meta_pred'(call_with_args, 10, call_with_args(9, ?, ?, ?, ?, ?, ?, ?, ?, ?)).
'$prop_meta_pred'(call_with_args, 11, call_with_args(10, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)).
'$prop_meta_pred'(clause, 2, clause(:, ?)).
'$prop_meta_pred'(consult, 1, consult(:)).
'$prop_meta_pred'('.', 2, '.'(:, +)).
'$prop_meta_pred'(current_predicate, 1, current_predicate(:)).
'$prop_meta_pred'(findall, 3, findall(?, 0, -)).
'$prop_meta_pred'(forall, 2, forall(0, 0)).
'$prop_meta_pred'(nospy, 1, nospy(:)).
%'$prop_meta_pred'(format, 2, format(+, :)).
%'$prop_meta_pred'(format, 3, format(+, +, :)).
'$prop_meta_pred'(listing, 1, listing(:)).
'$prop_meta_pred'(once, 1, once(0)).
'$prop_meta_pred'(phrase, 2, phrase(2, ?)).
'$prop_meta_pred'(phrase, 3, phrase(2, ?, ?)).
'$prop_meta_pred'(predicate_property, 2, predicate_property(:, ?)).
'$prop_meta_pred'(retract, 1, retract(:)).
'$prop_meta_pred'(retractall, 1, retractall(:)).
'$prop_meta_pred'(setof, 3, setof(?, 0, -)).
'$prop_meta_pred'(spy, 1, spy(:)).

'$prop_meta_pred'(fd_minimize, 2, fd_minimize(0, ?)).
'$prop_meta_pred'(fd_maximize, 2, fd_maximize(0, ?)).


'$get_pred_indicator'(PI, Func, Arity) :-
	'$call_c_test'('Pl_Get_Pred_Indicator_3'(PI, Func, Arity)).




'$get_predicate_file_info'(PI, PlFile, PlLine) :-
	'$call_c_test'('Pl_Get_Predicate_File_Info_3'(PI, PlFile, PlLine)).




'$set_predicate_file_info'(PI, PlFile, PlLine) :-
	'$call_c_test'('Pl_Set_Predicate_File_Info_3'(PI, PlFile, PlLine)).




'$aux_name'(Name) :-
	'$call_c_test'('Pl_Aux_Name_1'(Name)).




'$father_of_aux_name'(Name, FatherName, FatherArity) :-
	'$call_c_test'('Pl_Father_Of_Aux_Name_3'(Name, FatherName, FatherArity)).




'$pred_without_aux'(Name, Arity, Name1, Arity1) :-
	'$call_c_test'('Pl_Pred_Without_Aux_4'(Name, Arity, Name1, Arity1)).




'$make_aux_name'(Name, Arity, AuxNb, AuxName) :-
	'$call_c_test'('Pl_Make_Aux_Name_4'(Name, Arity, AuxNb, AuxName)).
