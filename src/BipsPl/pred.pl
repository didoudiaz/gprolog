/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : pred.pl                                                         *
 * Descr.: predicate manipulation management                               *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2014 Daniel Diaz                                     *
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

current_module(Module) :-
	set_bip_name(current_module, 1),
	'$current_module'(Module).

'$current_module'(Module) :-
	'$call_c_test'('Pl_Current_Module_2'(Module, 0)).
	

'$current_module_alt' :-         % used by C code to create a choice-point
	'$call_c_test'('Pl_Current_Module_Alt_0').




'$current_module_relax'(Module) :-
	atom(Module), !.	% accept any atom (does not fail if not exists)

'$current_module_relax'(Module) :-
	'$current_module'(Module).




'$select_module'(0, Module, Module).   		% 0 = module only

'$select_module'(1, Module, Module) :- 		% 1 = module + user
	(   Module \== user, Module \== system ->
	    true
	;
	    !
	).
	
'$select_module'(1, _, user).

'$select_module'(2, Module, Module) :- 		% 2 = module + user + system
	Module \== user,
	Module \== system.

'$select_module'(2, Module, user) :-
	Module \== system.

'$select_module'(2, _, system).




:- meta_predicate(current_predicate(:)).

current_predicate(MPI) :-
	set_bip_name(current_predicate, 1),
	'$current_predicate'(MPI, 0, 0).

:- meta_predicate('$current_predicate'(:)).

'$current_predicate'(MPI) :-
	'$current_predicate'(MPI, 0, 0).

:- meta_predicate('$current_predicate_any'(:)).

'$current_predicate_any'(MPI) :-
	'$current_predicate'(MPI, 2, 1). % also dollar


	/* '$current_predicate' expects a qualified PI */
			% WhichModule = 0=only given, 1=include user, 2=include user and system
			% AlsoMask = Import(0/1)  Dollar(0/1)
'$current_predicate'(MPI, WhichModule, AlsoMask) :-
	'$strip_module_var'(MPI, HomeModule, PI),
%write('home_module'(HomeModule)),nl,
	'$current_module_relax'(HomeModule),
%write('home_module'(HomeModule)),nl,
	'$select_module'(WhichModule, HomeModule, Module),
%write('module'(Module)),nl,
	'$call_c_test'('Pl_Current_Predicate_4'(Module, HomeModule, PI, AlsoMask)).


'$current_predicate_alt' :-         % used by C code to create a choice-point
        '$call_c_test'('Pl_Current_Predicate_Alt_0').




/* From 1.4.0 predicate_property only accepts a Head which is a callable.
 * In previous versions a predicate_indicator was expected, a callable was
 * accepted iff strict_iso was off.
 * This is no longer the case.
 */


:- meta_predicate(predicate_property(:, ?)).

predicate_property(MHead, Property) :-
	set_bip_name(predicate_property, 2),
	'$predicate_property'(MHead, Property, 2, 0).

:- meta_predicate('$predicate_property'(:, ?)).

'$predicate_property'(MHead, Property) :-
	'$predicate_property'(MHead, Property, 2, 0).

:- meta_predicate('$predicate_property_any'(:, ?)).

'$predicate_property_any'(MHead, Property) :-
	'$predicate_property'(MHead, Property, 2, 1). % also dollar


	/* '$predicate_property' expects a qualified head */
			% WhichModule = 0=only given, 1=include user, 2=include user and system
			% AlsoMask = Import(0/1)  Dollar(0/1)
'$predicate_property'(MHead, Property, WhichModule, AlsoMask) :-
	'$strip_module_var'(MHead, HomeModule, Head),
	'$current_module_relax'(HomeModule),
	'$select_module'(WhichModule, HomeModule, Module),
	'$head_to_fn'(Head, F, N),
	'$call_c_test'('Pl_Current_Predicate_4'(Module, HomeModule, F/N, AlsoMask)),
	functor(Head, F, N),
	(   '$predicate_property1'(Module, F, N, Property)
	;
	    HomeModule \== Module, Property = imported_from(Module)
	).



'$head_to_fn'(Head, F, N) :-
	callable(Head), !,
	functor(Head, F, N).

'$head_to_fn'(Head, _, _) :-
	var(Head), !.

'$head_to_fn'(Head, _, _) :-
	'$pl_err_type'(callable, Head).




% FIXME: when stable remove it (used in read_file.pl/syn_sugar.pl
% to test built_in, pass system in both cases)
'$predicate_property1'(Func, Arity, Property) :-
	'$predicate_property1'(system, Func, Arity, Property).


				% here Module, Func, Arity are instantiated

'$predicate_property1'(Module, Func, Arity, Property) :-
	'$check_pred_prop'(Property), !,
	'$predicate_property2'(Property, Module, Func, Arity).




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

'$check_pred_prop'(imported_from(_)).

'$check_pred_prop'(Property) :-
	'$pl_err_domain'(predicate_property, Property).




'$predicate_property2'(static, Module, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Static_2'(Module, Func, Arity)).

'$predicate_property2'(dynamic, Module, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Dynamic_2'(Module, Func, Arity)).

'$predicate_property2'(private, Module, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Private_2'(Module, Func, Arity)).

'$predicate_property2'(public, Module, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Public_2'(Module, Func, Arity)).

'$predicate_property2'(monofile, Module, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Monofile_2'(Module, Func, Arity)).

'$predicate_property2'(multifile, Module, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Multifile_2'(Module, Func, Arity)).

'$predicate_property2'(user, Module, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_User_2'(Module, Func, Arity)).

'$predicate_property2'(built_in, Module, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Built_In_2'(Module, Func, Arity)).

'$predicate_property2'(built_in_fd, Module, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Built_In_Fd_2'(Module, Func, Arity)).

'$predicate_property2'(control_construct, Module, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Control_Construct_2'(Module, Func, Arity)).

'$predicate_property2'(native_code, Module, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Native_Code_2'(Module, Func, Arity)).

'$predicate_property2'(prolog_file(PlFile), Module, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Prolog_File_3'(Module, Func, Arity, PlFile)).

'$predicate_property2'(prolog_line(PlLine), Module, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Prolog_Line_3'(Module, Func, Arity, PlLine)).

'$predicate_property2'(meta_predicate(MetaPredSpec), Module, Func, Arity) :-
	'$call_c_test'('Pl_Pred_Prop_Meta_Predicate_3'(Module, Func, Arity, MetaPredSpec)).




'$get_predicate_file_info'(Module, F, N, PlFile, PlLine) :-
	'$call_c_test'('Pl_Get_Predicate_File_Info_5'(Module, F, N, PlFile, PlLine)).




'$set_predicate_file_info'(Module, F, N, PlFile, PlLine) :-
	'$call_c_test'('Pl_Set_Predicate_File_Info_5'(Module, F, N, PlFile, PlLine)).




'$get_module_of_pred'(HomeModule, Func, Arity, Module) :-
	'$call_c_test'('Pl_Get_Module_Of_Pred_4'(HomeModule, Func, Arity, Module)).


'$get_module_of_goal'(HomeModule, Goal, Module) :-
	'$call_c_test'('Pl_Get_Module_Of_Goal_3'(HomeModule, Goal, Module)).
	
'$get_module_of_goal_if_not_meta_pred'(HomeModule, Goal, Module) :-
	'$call_c_test'('Pl_Get_Module_Of_Goal_If_Not_Meta_Pred_3'(HomeModule, Goal, Module)).
	




'$aux_name'(Name) :-
	'$call_c_test'('Pl_Aux_Name_1'(Name)).




'$father_of_aux_name'(Name, FatherName, FatherArity) :-
	'$call_c_test'('Pl_Father_Of_Aux_Name_3'(Name, FatherName, FatherArity)).




'$pred_without_aux'(Name, Arity, Name1, Arity1) :-
	'$call_c_test'('Pl_Pred_Without_Aux_4'(Name, Arity, Name1, Arity1)).




'$make_aux_name'(Name, Arity, AuxNb, AuxName) :-
	'$call_c_test'('Pl_Make_Aux_Name_4'(Name, Arity, AuxNb, AuxName)).



'$declare_meta_predicate'(Module, MetaPredSpec) :-
	'$call_c_test'('Pl_Decl_Meta_Pred_2'(Module, MetaPredSpec)).


:- meta_predicate(strip_module(:, ?, ?)).

strip_module(Term, Module, Plain) :-
	'$call_c_test'('Pl_Strip_Module_3'(Term, Module, Plain)).

				% these 2 sets the calling module
'$strip_module_var'(Term, Module, Plain) :-
	'$call_c_test'('Pl_Strip_Module_Var_3'(Term, Module, Plain)).


'$strip_module_nonvar'(Term, Module, Plain) :-
	'$call_c_test'('Pl_Strip_Module_Nonvar_3'(Term, Module, Plain)).
