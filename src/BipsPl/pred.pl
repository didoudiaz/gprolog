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

'$check_pred_prop'(user).

'$check_pred_prop'(built_in).

'$check_pred_prop'(built_in_fd).

'$check_pred_prop'(native_code).

'$check_pred_prop'(prolog_file(_)).

'$check_pred_prop'(prolog_line(_)).

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
