/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : src_rdr.pl                                                      *
 * Descr.: Prolog source file reader                                       *
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

'$use_src_rdr'.

/* API

setof(Y:X,(predicate_property(X, prolog_file(F)), decompose_file_name(F, _, src_rdr, '.pl'), predicate_property(X, prolog_line(Y))), L), member(_:A, L), functor(A,Func,Arity), write(Func/Arity), nl, fail.


sr_open/3
sr_change_options/2
sr_close/1
sr_read_term/4
sr_current_descriptor/1
sr_get_stream/2
sr_get_module/3
sr_get_file_name/2
sr_get_position/3
sr_get_include_list/2
sr_get_include_stream_list/2
sr_get_size_counters/3
sr_get_error_counters/3
sr_set_error_counters/3
sr_write_message/4
sr_write_message/6
sr_write_message/8
sr_write_error/2
sr_write_error/4
sr_write_error/6
sr_error_from_exception/2
*/

sr_open(FileOrStream, D, Options) :-
	set_bip_name(sr_open, 3),
        '$set_sr_defaults',
        '$get_sr_options'(Options, OutSorA),
	(   var(D) ->
	    true
	;   '$pl_err_type'(variable, D)
	),
	'$call_c'('Pl_SR_Init_Open_2'(D, OutSorA)),
	(   nonvar(FileOrStream), FileOrStream = '$stream'(_) ->
	    '$call_c'('Pl_SR_Open_File_2'(FileOrStream, true))
	;
	    '$sr_open_new_prolog_file'(FileOrStream)
	).




          % option mask in sys_var[0]:
          %
          % include         in b1/b0        treat/pass
          % op              in b3/b2          0 / 0 = kill
          % set_prolog_flag in b5/b4  code:   0 / 1 = ignore
          % char_conversion in b7/b6          1 / 0 = hide
          % module          in b9/b8          1 / 1 = reflect
	  %
          % restart         in b16 (0/1)
          % reflect_eof     in b17 (0/1)
          % undo_directives in b18 (0/1)
          % write_error     in b19 (0/1)
          %
          % in sys_var[1]: is output stream specified ?

'$set_sr_defaults' :-
        '$sys_var_write'(0, 0b1111111110),                     % default mask
	'$sys_var_set_bit'(0, 19),
        '$sys_var_write'(1, 0).


'$get_sr_options'(Options, OutSorA) :-
        '$check_list'(Options),
	g_assign('$sr_output_stream', 0),
        '$get_sr_options1'(Options),
	g_read('$sr_output_stream', OutSorA).


'$get_sr_options1'([]).

'$get_sr_options1'([X|Options]) :-
        '$get_sr_options2'(X), !,
        '$get_sr_options1'(Options).


'$get_sr_options2'(X) :-
        var(X),
        '$pl_err_instantiation'.

'$get_sr_options2'(T) :-
	functor(T, F, 1),
	arg(1, T, A),
	nonvar(A),
	'$sr_treat_pass_no'(F, _, SubMaskPos),
	BitPass is SubMaskPos * 2,
	BitTreat is BitPass + 1,
	set_bip_name(sr_open, 3), % due to the use of is/2
	'$sr_set_treat_pass_bits'(A, BitPass, BitTreat).

'$get_sr_options2'(restart(X)) :-
        nonvar(X),
        (   X = false,
            '$sys_var_reset_bit'(0, 16)
        ;   X = true,
            '$sys_var_set_bit'(0, 16)
        ).

'$get_sr_options2'(reflect_eof(X)) :-
        nonvar(X),
        (   X = false,
            '$sys_var_reset_bit'(0, 17)
        ;   X = true,
            '$sys_var_set_bit'(0, 17)
        ).

'$get_sr_options2'(undo_directives(X)) :-
        nonvar(X),
        (   X = false,
            '$sys_var_reset_bit'(0, 18)
        ;   X = true,
            '$sys_var_set_bit'(0, 18)
        ).

'$get_sr_options2'(write_error(X)) :-
        nonvar(X),
        (   X = false,
            '$sys_var_reset_bit'(0, 19)
        ;   X = true,
            '$sys_var_set_bit'(0, 19)
        ).

'$get_sr_options2'(output_stream(SorA)) :-
	g_link('$sr_output_stream', SorA),
        '$sys_var_write'(1, 1).

'$get_sr_options2'(X) :-
        '$pl_err_domain'(sr_option, X).




	  % '$sr_treat_pass_no'(Name, Arity, SubMaskPos)

'$sr_treat_pass_no'(include, 1, 0).
'$sr_treat_pass_no'(op, 3, 1).
'$sr_treat_pass_no'(set_prolog_flag, 2, 2).
'$sr_treat_pass_no'(char_conversion, 2, 3).
'$sr_treat_pass_no'(module, 1, 4).
'$sr_treat_pass_no'(end_module, 1, 4).
'$sr_treat_pass_no'(body, 1, 4).
'$sr_treat_pass_no'(end_body, 1, 4).




'$sr_set_treat_pass_bits'(kill, BitPass, BitTreat) :-
	 '$sys_var_reset_bit'(0, BitPass),
	 '$sys_var_reset_bit'(0, BitTreat).

'$sr_set_treat_pass_bits'(ignore, BitPass, BitTreat) :-
	 '$sys_var_set_bit'(0, BitPass),
	 '$sys_var_reset_bit'(0, BitTreat).

'$sr_set_treat_pass_bits'(hide, BitPass, BitTreat) :-
	 '$sys_var_reset_bit'(0, BitPass),
	 '$sys_var_set_bit'(0, BitTreat).

'$sr_set_treat_pass_bits'(reflect, BitPass, BitTreat) :-
	 '$sys_var_set_bit'(0, BitPass),
	 '$sys_var_set_bit'(0, BitTreat).




'$sr_open_new_prolog_file'(File) :-
	'$call_c'('Pl_Prolog_File_Name_2'(File, File1)),
	'$call_c'('Pl_SR_Open_File_2'(File1, false)).
/*
	(   '$call_c_test'('Pl_File_Permission_2'(File, [read])) ->
	    File1 = File
	;
	    '$call_c'('Pl_Prolog_File_Name_2'(File, File1))
	  ),
*/


sr_change_options(D, Options) :-
	set_bip_name(sr_change_options, 2),
	'$call_c'('Pl_SR_Check_Descriptor_1'(D)), % also init sys_var[0]
        '$get_sr_options1'(Options),
	'$call_c'('Pl_SR_Change_Options_0').




sr_close(D) :-
	set_bip_name(sr_close, 1),
	'$call_c'('Pl_SR_Close_1'(D)).



sr_new_pass(D) :-
	set_bip_name(sr_new_pass, 1),
	(   '$call_c_test'('Pl_SR_New_Pass_1'(D)) ->
	    true
	;   '$pl_err_permission'(new_pass, one_pass_reader, D)
	).




sr_read_term(D, Term, Options, SRError) :-
	'$call_c'('Pl_SR_Check_Descriptor_1'(D)),
	repeat,
	'$call_c'('Pl_SR_Get_Stm_For_Read_Term_1'(Stm)),
	Stream = '$stream'(Stm),
	set_bip_name(sr_read_term, 3),
	'$catch'('$read_term'(Stream, Term, Options), Excep, true,
		 sr_read_term, 3, false),
	'$call_c'('Pl_SR_Update_Position_0'),
	(   var(Excep) ->
	    '$sr_treat_term'(Term, SRError)
	;   Term = '$sr_read_term_error',
	    '$sr_error_from_exception'(Excep, SRError)
	),
	(   SRError = sr_error(_, _),
	    '$call_c_test'('Pl_SR_Is_Bit_Set_1'(19)) ->
	    sr_write_error(D, SRError)
	;   true
	), !.			% cut to remove repeat choice-point




	%% '$sr_treat_term'(Term, SRError) handles a read term
	%% It can fail to enforce backtracking and next term reading.
	%% Warning: Term can be a variable - should not be altered.

'$sr_treat_term'(Term, SRError) :-
	Term == end_of_file,
	!,			% cut to backtrack to repeat
	'$call_c_test'('Pl_SR_EOF_Reached_1'(Err)), % this one can fail
	(   var(Err) ->
	    SRError = sr_ok
	;   SRError = sr_error(warning, Err)).

'$sr_treat_term'(Term, SRError) :-
	nonvar(Term),
	Term = (:- Directive),
	nonvar(Directive),
	functor(Directive, F, A),
	'$sr_treat_pass_no'(F, A, SubMaskPos),
	!,			% cut to backtrack to repeat
	BitPass is SubMaskPos * 2,
	BitTreat is BitPass + 1,
	(   '$call_c_test'('Pl_SR_Is_Bit_Set_1'(BitTreat)) ->
	    '$catch'('$sr_exec_directive'(Directive, SRError),
		     Excep,
		     '$sr_error_from_exception'(Excep, SRError),
		     any, 0, false)
	;
	    true),
	(   var(Excep) ->
	    '$call_c_test'('Pl_SR_Is_Bit_Set_1'(BitPass)) % can fail
	;   true).

'$sr_treat_term'(_, sr_ok).




'$sr_exec_directive'(Directive, SRError) :-
	'$sr_directive1'(Directive, SRError),
	!.

'$sr_exec_directive'(_, SRError) :-
	SRError = sr_error(warning, 'directive failed').




'$sr_directive1'(include(File), sr_ok) :-
	'$sr_open_new_prolog_file'(File).

'$sr_directive1'(op(Prec, Specif, Oper), sr_ok) :-
	(   nonvar(Specif),
	    nonvar(Oper),
	    '$sr_op_type'(Specif, OpType),
	    current_op(OldPrec, OldSpecif, Oper),
	    '$sr_op_type'(OldSpecif, OpType) ->
	    true
	;   OldPrec = 0,
	    OldSpecif = Specif
	),
	'$call_c'('Pl_SR_Add_Directive_7'(0,
				       Prec, Specif, Oper,
				       OldPrec, OldSpecif, Oper)).

'$sr_directive1'(set_prolog_flag(Flag, Value), sr_ok) :-
	(   nonvar(Flag), current_prolog_flag(Flag, OldValue) ->
	    true
	;   true
	),
	'$call_c'('Pl_SR_Add_Directive_7'(1,
				       Flag, Value, 0,
				       Flag, OldValue, 0)).

'$sr_directive1'(char_conversion(InChar, OutChar), sr_ok) :-
	(   nonvar(InChar), current_char_conversion(InChar, OldOutChar) ->
	    true
	;   OldOutChar = InChar
	),
	'$call_c'('Pl_SR_Add_Directive_7'(2,
				       InChar, OutChar, 0,
				       InChar, OldOutChar, 0)).

'$sr_directive1'(module(ModuleName), SRError) :-
	'$sr_start_module'(ModuleName, true, SRError).

'$sr_directive1'(body(ModuleName), SRError) :-
	'$sr_start_module'(ModuleName, false, SRError).

'$sr_directive1'(end_module(ModuleName), SRError) :-
	'$sr_stop_module'(ModuleName, true, SRError).

'$sr_directive1'(end_body(ModuleName), SRError) :-
	'$sr_stop_module'(ModuleName, false, SRError).




'$sr_op_type'(fx, prefix).
'$sr_op_type'(fy, prefix).
'$sr_op_type'(xfx, infix).
'$sr_op_type'(yfx, infix).
'$sr_op_type'(xfy, infix).
'$sr_op_type'(xf, postfix).
'$sr_op_type'(yf, postfix).




'$sr_start_module'(ModuleName, ModulePart, SRError) :-
	'$call_c'('Pl_SR_Start_Module_3'(ModuleName, ModulePart, Err)),
	(   var(Err) ->
	    SRError = sr_ok
	;   SRError = sr_error(warning, Err)).




'$sr_stop_module'(ModuleName, ModulePart, SRError) :-
	'$call_c'('Pl_SR_Stop_Module_3'(ModuleName, ModulePart, Err)),
	(   var(Err) ->
	    SRError = sr_ok
	;   SRError = sr_error(warning, Err)).




sr_current_descriptor(D) :-
	set_bip_name(sr_current_descriptor, 1),
	'$call_c_test'('Pl_SR_Current_Descriptor_1'(D)).




'$sr_current_descriptor_alt' :-	% used by C code to create a choice-point
	'$call_c_test'('Pl_SR_Current_Descriptor_Alt_0').




sr_get_stream(D, Stream) :-
	set_bip_name(sr_get_stream, 2),
	'$check_stream_or_var'(Stream, Stm),
	'$call_c_test'('Pl_SR_Get_Stm_2'(D, Stm)).




sr_get_module(D, ModuleName, ModulePart) :-
	set_bip_name(sr_get_module, 3),
	'$call_c_test'('Pl_SR_Get_Module_3'(D, ModuleName, ModulePart)).




sr_get_file_name(D, File) :-
	set_bip_name(sr_get_file_name, 2),
	'$call_c_test'('Pl_SR_Get_File_Name_2'(D, File)).




sr_get_position(D, L1, L2) :-
	set_bip_name(sr_get_position, 3),
	'$call_c_test'('Pl_SR_Get_Position_3'(D, L1, L2)).




sr_get_include_list(D, IncList) :-
	set_bip_name(sr_get_include_list, 2),
	'$call_c_test'('Pl_SR_Get_Include_List_2'(D, IncList)).




sr_get_include_stream_list(D, IncStreamList) :-
	set_bip_name(sr_get_include_stream_list, 2),
	'$call_c_test'('Pl_SR_Get_Include_Stream_List_2'(D, IncStreamList)).




sr_get_size_counters(D, Chars,  Lines) :-
	set_bip_name(sr_get_size_counters, 3),
	'$call_c_test'('Pl_SR_Get_Size_Counters_3'(D, Chars, Lines)).




sr_get_error_counters(D, Errors,  Warnings) :-
	set_bip_name(sr_get_error_counters, 3),
	'$call_c_test'('Pl_SR_Get_Error_Counters_3'(D, Errors, Warnings)).




sr_set_error_counters(D, Errors,  Warnings) :-
	set_bip_name(sr_set_error_counters, 3),
	'$call_c'('Pl_SR_Set_Error_Counters_3'(D, Errors, Warnings)).




sr_write_message(D, Type, Format, Args) :-
	set_bip_name(sr_write_message, 4),
	'$call_c'('Pl_SR_Write_Message_4'(D, Type, Format, Args)).




sr_write_message(D, L1, L2C, Type, Format, Args) :-
	set_bip_name(sr_write_message, 6),
	'$call_c'('Pl_SR_Write_Message_6'(D, L1, L2C, Type, Format, Args)).




sr_write_message(D, IncList, File, L1, L2C, Type, Format, Args) :-
	set_bip_name(sr_write_message, 8),
	'$call_c'('Pl_SR_Write_Message_8'(D, IncList, File, L1, L2C,
				       Type, Format, Args)).



sr_write_error(D, SRError) :-
	set_bip_name(sr_write_error, 2),
	'$sr_get_format_args_error'(SRError, L1, L2C, Type, Format, Args),
	(   var(L1),
	    '$call_c'('Pl_SR_Write_Message_4'(D, Type, Format, Args))
	;
	    '$call_c'('Pl_SR_Write_Message_6'(D, L1, L2C, Type, Format, Args))
	), !.

sr_write_error(_, _).		% succes - nothing written for sr_ok




sr_write_error(D, L1, L2C, SRError) :-
	set_bip_name(sr_write_error, 4),
	'$sr_get_format_args_error'(SRError, EL1, EL2C, Type, Format, Args),
	(   L1 = EL1, L2C = EL2C  ; true ),
	'$call_c'('Pl_SR_Write_Message_6'(D, EL1, EL2C, Type, Format, Args)), !.

sr_write_error(_, _, _, _).	% succes - nothing written for sr_ok




sr_write_error(D, IncList, File, L1, L2C, SRError) :-
	set_bip_name(sr_write_error, 6),
	'$sr_get_format_args_error'(SRError, EL1, EL2C, Type, Format, Args),
	(   L1 = EL1, L2C = EL2C  ; true ),
	'$call_c'('Pl_SR_Write_Message_8'(D, IncList, File, EL1, EL2C,
				       Type, Format, Args)), !.

sr_write_error(_, _, _, _, _, _). % succes - nothing written for sr_ok




'$sr_get_format_args_error'(SRError, _, _, _, _, _) :-
	var(SRError),
	'$pl_err_instantiation'.

'$sr_get_format_args_error'(SRError, L1, L2C, Type, Format, Args) :-
	SRError = sr_error(Type, Error), % fail for sr_ok
	'$sr_simpl_error'(Error, L1, L2C, Format, Args).




'$sr_simpl_error'(syntax(Line, Char, Error), Line, L2C, Format, Args) :-
	!,
	L2C is -Char,
	Format = '~a~n',
	Args = [Error].

'$sr_simpl_error'(existence_error(source_sink, F), _, _, Format, Args) :-
	!,
	Format = 'cannot open file ~a - does not exist~n',
	Args = [F].

'$sr_simpl_error'(permission_error(open, source_sink, F), _, _, Format, Args) :-
	!,
	Format = 'cannot open file ~a - permission error~n',
	Args = [F].

'$sr_simpl_error'(Error, _, _, Format, Args) :-
	Format = '~w~n',
	Args = [Error].




sr_error_from_exception(Excep, SRError) :-
	set_bip_name(sr_error_from_exception, 2),
	(   var(Excep) ->
	    '$pl_err_instantiation'
	;   true
	),
	'$sr_error_from_exception'(Excep, SRError).


'$sr_error_from_exception'(error(syntax_error(_), _), SRError) :-
	!,
	syntax_error_info(_, Line, Char, Error),
	SRError = sr_error(error, syntax(Line, Char, Error)).

'$sr_error_from_exception'(error(Excep, _), sr_error(error, Excep)) :-
	!.

'$sr_error_from_exception'(Excep, sr_error(exception, Excep)).
