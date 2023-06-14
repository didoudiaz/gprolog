/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : pred.pl                                                         *
 * Descr.: predicate manipulation management                               *
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

'$use_pred'.


:- meta_predicate(current_predicate(:)).

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



/* From 1.4.0 predicate_property only accepts a Head which is a callable.
 * In previous versions a predicate_indicato was expected, a callable was
 * accepted iff strict_iso was off.
 * This is no longer the case. We kept old version renamed '$predicate_property_pi'
 */

:- meta_predicate(predicate_property(:, ?)).

predicate_property(Head, Property) :-
	set_bip_name(predicate_property, 2),
	callable(Head), !,
	functor(Head, F, N),
	'$predicate_property1'(F, N, Property).

predicate_property(Head, Property) :-
	var(Head), !,
	'$current_predicate_bips'(F/N),
	functor(Head, F, N),
	'$predicate_property1'(F, N, Property).

predicate_property(Head, _) :-
	'$pl_err_type'(callable, Head).




'$predicate_property_pi'(PI, Property) :-
	'$current_predicate_bips'(PI),
	PI = Func/Arity,
	'$predicate_property1'(Func, Arity, Property).


'$predicate_property_pi_any'(PI, Property) :-
	'$current_predicate_any'(PI),
	PI = Func/Arity,
	'$predicate_property1'(Func, Arity, Property).




'$predicate_property1'(Func, Arity, Property) :-
	'$check_pred_prop'(Property), !,
	'$predicate_property2'(Property, Func, Arity).




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

'$check_pred_prop'(control_construct).

'$check_pred_prop'(native_code).

'$check_pred_prop'(prolog_file(_)).

'$check_pred_prop'(prolog_line(_)).

'$check_pred_prop'(meta_predicate(_)).

'$check_pred_prop'(Property) :-
	'$pl_err_domain'(predicate_property, Property).




'$predicate_property2'(static, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Static_2'(Func, Arity)).

'$predicate_property2'(dynamic, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Dynamic_2'(Func, Arity)).

'$predicate_property2'(private, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Private_2'(Func, Arity)).

'$predicate_property2'(public, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Public_2'(Func, Arity)).

'$predicate_property2'(monofile, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Monofile_2'(Func, Arity)).

'$predicate_property2'(multifile, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Multifile_2'(Func, Arity)).

'$predicate_property2'(user, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_User_2'(Func, Arity)).

'$predicate_property2'(built_in, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Built_In_2'(Func, Arity)).

'$predicate_property2'(built_in_fd, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Built_In_Fd_2'(Func, Arity)).

'$predicate_property2'(control_construct, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Control_Construct_2'(Func, Arity)).

'$predicate_property2'(native_code, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Native_Code_2'(Func, Arity)).

'$predicate_property2'(prolog_file(PlFile), Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Prolog_File_3'(Func, Arity, PlFile)).

'$predicate_property2'(prolog_line(PlLine), Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Prolog_Line_3'(Func, Arity, PlLine)).

'$predicate_property2'(meta_predicate(MetaPredTerm), Func, Arity) :-
	'$prop_meta_pred'(Func, Arity, MetaPredTerm).




% the control constructs (they are now found by predicate_property/2)
% their meta_predicate property is defined with a directive
% :- meta_predicate directive (see read_file.pl for compilation)
% before it was directly with the following facts

% '$prop_meta_pred'(',', 2, ','(0,0)).
% '$prop_meta_pred'(;, 2, ;(0,0)).
% '$prop_meta_pred'(->, 2, ->(0,0)).
% '$prop_meta_pred'(*->, 2, *->(0,0)).
% '$prop_meta_pred'(call, 1, call(0)).
% '$prop_meta_pred'(catch, 3, catch(0, ?, 0)).

:- meta_predicate(','(0,0)).
:- meta_predicate(;(0,0)).
:- meta_predicate(->(0,0)).
:- meta_predicate(*->(0,0)).
:- meta_predicate(call(0)).
:- meta_predicate(catch(0, ?, 0)).




'$get_pred_indicator'(PI, Func, Arity) :-
	'$call_c_test'('Pl_Get_Pred_Indicator_3'(PI, Func, Arity)).




'$get_predicate_file_info'(PI, PlFile, PlLine) :-
	'$call_c_test'('Pl_Get_Predicate_File_Info_3'(PI, PlFile, PlLine)).




'$set_predicate_file_info'(PI, PlFile, PlLine) :-
	'$call_c_test'('Pl_Set_Predicate_File_Info_3'(PI, PlFile, PlLine)).




'$aux_name'(Name) :-
	'$call_c_test'('Pl_Aux_Name_1'(Name)).




'$not_aux_name'(Name) :-
	'$call_c_test'('Pl_Not_Aux_Name_1'(Name)).




'$father_of_aux_name'(Name, FatherName, FatherArity) :-
	'$call_c_test'('Pl_Father_Of_Aux_Name_3'(Name, FatherName, FatherArity)).




'$pred_without_aux'(Name, Arity, Name1, Arity1) :-
	'$call_c_test'('Pl_Pred_Without_Aux_4'(Name, Arity, Name1, Arity1)).




'$make_aux_name'(Name, Arity, AuxNb, AuxName) :-
	'$call_c_test'('Pl_Make_Aux_Name_4'(Name, Arity, AuxNb, AuxName)).
