/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : flag.pl                                                         *
 * Descr.: Prolog flag and system variable management                      *
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

'$use_flag'.


set_prolog_flag(Flag, Value) :-
	set_bip_name(set_prolog_flag, 2),
	'$call_c_test'('Pl_Set_Prolog_Flag_2'(Flag, Value)).




current_prolog_flag(Flag, Value) :-
	set_bip_name(current_prolog_flag, 2),
	'$call_c_test'('Pl_Current_Prolog_Flag_2'(Flag, Value)).




'$current_prolog_flag_alt' :-       % used by C code to create a choice-point
	'$call_c_test'('Pl_Current_Prolog_Flag_Alt_0').




'$sys_var_write'(Var, N) :-
	'$call_c'('Pl_Sys_Var_Write_2'(Var, N)).

'$sys_var_read'(Var, N) :-
	'$call_c_test'('Pl_Sys_Var_Read_2'(Var, N)).





'$sys_var_inc'(Var) :-
	'$call_c'('Pl_Sys_Var_Inc_1'(Var)).

'$sys_var_dec'(Var) :-
	'$call_c'('Pl_Sys_Var_Dec_1'(Var)).




'$sys_var_set_bit'(Var, Bit) :-
	'$call_c'('Pl_Sys_Var_Set_Bit_2'(Var, Bit)).

'$sys_var_reset_bit'(Var, Bit) :-
	'$call_c'('Pl_Sys_Var_Reset_Bit_2'(Var, Bit)).

'$sys_var_get_bit'(Var, Bit, Value) :-
	'$call_c_test'('Pl_Sys_Var_Get_Bit_3'(Var, Bit, Value)).




'$sys_var_put'(Var, Term) :-
	'$call_c'('Pl_Sys_Var_Put_2'(Var, Term)).

'$sys_var_get'(Var, Term) :-
	'$call_c_test'('Pl_Sys_Var_Get_2'(Var, Term)).




'$get_current_B'(X) :-
	'$call_c'('Pl_Get_Current_B_1'(X)).

'$set_current_B'(X) :-
	'$call_c'('Pl_Set_Current_B_1'(X)).


write_pre_load_file(File) :-
	set_bip_name(write_pre_load_file, 1),
	'$open'(File, write, S, []),
	(   setof(Op, (current_op(Prior,Prec,Op), Op \== (',')), LOp),
	    '$write_pre_load_goal'(S, op(Prior, Prec, LOp)),
	    fail
	;
	    true
	),
	LFlags = [char_conversion, double_quotes, back_quotes, singleton_warning,
		  suspicious_warning, multifile_warning, strict_iso, show_information],
	(   member(Flag, LFlags),
	    current_prolog_flag(Flag, FlagValue),
	    '$write_pre_load_goal'(S, set_prolog_flag(Flag, FlagValue)),
	    fail
	;
	    true
	),
	'$sys_var_read'(20, SysVar), % SYS_VAR_SAY_GETC
	'$write_pre_load_goal'(S, '$sys_var_write'(20, SysVar)),
	(   current_char_conversion(Ch1,Ch2),
	    '$write_pre_load_goal'(S, char_conversion(Ch1, Ch2)),
	    fail
	;
	    true
	),
	(   TermHead = term_expansion(_, _),
	    catch(('$clause'(TermHead, TermBody, 2), portray_clause(S, (TermHead :- TermBody)), fail), _, fail)
	;
	    true
	),
	close(S),
	fail.			% GC

write_pre_load_file(_).


'$write_pre_load_goal'(S, Goal) :-
	portray_clause(S, (:- initialization(Goal))).



	% basically it a consult(File) but 1) as it is used in consult+pl2wam
	% this leads to an infinite recursion and 2) it has to handle pass directives
	% other than initialization/1 to add_input_term/4.
	% So we simply read-assert/execute the content.

read_pre_load_file(File) :-
	set_bip_name(read_pre_load_file, 1),        
	(   file_exists(File) ->
	    File1 = File
	;   prolog_file_name(File, File1)
	),
	absolute_file_name(File1, File2),
	'$open'(File2, read, S, []),
	repeat,
	last_read_start_line_column(L1, _),
	catch(read(S, Cl), Err, (format('~a (pre-load file) read caused exception ~w~n',
					[File2, Err]), fail)),
	stream_line_column(S, Line, Col),
	(   Col = 1 ->
	    L2 is Line - 1
	;   L2 = Line
	),
	(   Cl = end_of_file ->
	    true
	;   Cl = (:- initialization(Goal)) ->
	    catch(once(Goal), Err, format('~a:~d (pre-load file) directive ~w caused exception ~w~n',
					  [File2, L1, Cl, Err])),
	    fail
	;   Cl = (:- _) ->
	    current_predicate(add_input_term/4),
	    once(add_input_term(Cl, File2, L1, L2)),
	    fail
	;   catch(assertz(Cl), Err, format('~a:~d (pre-load file) assertz(~w) caused exception ~w~n',
					   [File2, L1, Cl, Err])),
	    fail
	), !,
	close(S).




argument_counter(N) :-
	set_bip_name(argument_counter, 1),
	'$call_c_test'('Pl_Argument_Counter_1'(N)).




argument_value(I, A) :-
	set_bip_name(argument_value, 2),
	'$call_c_test'('Pl_Argument_Value_2'(I, A)).




argument_list(List) :-
	set_bip_name(argument_list, 1),
	'$call_c_test'('Pl_Argument_List_1'(List)).


environ(VarName, Value) :-
	set_bip_name(environ, 2),
	'$call_c_test'('Pl_Environ_2'(VarName, Value)).


'$environ_alt' :-                   % used by C code to create a choice-point
	'$call_c_test'('Pl_Environ_Alt_0').
