/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog buit-in predicates                                       */
/* File  : flag.pl                                                         */
/* Descr.: Prolog flag and system variable management                      */
/* Author: Daniel Diaz                                                     */
/*                                                                         */
/* Copyright (C) 1999,2000 Daniel Diaz                                     */
/*                                                                         */
/* GNU Prolog is free software; you can redistribute it and/or modify it   */
/* under the terms of the GNU General Public License as published by the   */
/* Free Software Foundation; either version 2, or any later version.       */
/*                                                                         */
/* GNU Prolog is distributed in the hope that it will be useful, but       */
/* WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        */
/* General Public License for more details.                                */
/*                                                                         */
/* You should have received a copy of the GNU General Public License along */
/* with this program; if not, write to the Free Software Foundation, Inc.  */
/* 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     */
/*-------------------------------------------------------------------------*/

:-	built_in.

'$use_flag'.


set_prolog_flag(Flag, Value) :-
	set_bip_name(set_prolog_flag, 2),
	'$call_c_test'('Set_Prolog_Flag_2'(Flag, Value)).




current_prolog_flag(Flag, Value) :-
	set_bip_name(current_prolog_flag, 2),
	'$call_c_test'('Current_Prolog_Flag_2'(Flag, Value)).




'$current_prolog_flag_alt' :-                    % used by C code to create a choice-point
	'$call_c_test'('Current_Prolog_Flag_Alt_0').




'$sys_var_write'(Var, N) :-
	'$call_c'('Sys_Var_Write_2'(Var, N)).

'$sys_var_read'(Var, N) :-
	'$call_c_test'('Sys_Var_Read_2'(Var, N)).





'$sys_var_inc'(Var) :-
	'$call_c'('Sys_Var_Inc_1'(Var)).

'$sys_var_dec'(Var) :-
	'$call_c'('Sys_Var_Dec_1'(Var)).




'$sys_var_set_bit'(Var, Bit) :-
	'$call_c'('Sys_Var_Set_Bit_2'(Var, Bit)).

'$sys_var_reset_bit'(Var, Bit) :-
	'$call_c'('Sys_Var_Reset_Bit_2'(Var, Bit)).




'$sys_var_put'(Var, Term) :-
	'$call_c'('Sys_Var_Put_2'(Var, Term)).

'$sys_var_get'(Var, Term) :-
	'$call_c_test'('Sys_Var_Get_2'(Var, Term)).




'$get_current_B'(X) :-
	'$call_c'('Get_Current_B_1'(X)).

'$set_current_B'(X) :-
	'$call_c'('Set_Current_B_1'(X)).




write_pl_state_file(File) :-
	set_bip_name(write_pl_state_file, 1),
	'$call_c_test'('Write_Pl_State_File'(File)).




read_pl_state_file(File) :-
	set_bip_name(read_pl_state_file, 1),
	'$call_c_test'('Read_Pl_State_File'(File)).




argument_counter(N) :-
	set_bip_name(argument_counter, 1),
	'$call_c_test'('Argument_Counter_1'(N)).




argument_value(I, A) :-
	set_bip_name(argument_value, 2),
	'$call_c_test'('Argument_Value_2'(I, A)).




argument_list(List) :-
	set_bip_name(argument_list, 1),
	'$call_c_test'('Argument_List_1'(List)).


environ(VarName, Value) :-
	set_bip_name(environ, 2),
	'$call_c_test'('Environ_2'(VarName, Value)).


'$environ_alt' :-                                % used by C code to create a choice-point
	'$call_c_test'('Environ_Alt_0').




