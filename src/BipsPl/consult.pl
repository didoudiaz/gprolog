/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : consult.pl                                                      *
 * Descr.: file consulting                                                 *
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

'$use_consult'.


:- meta_predicate('.'(:, +)).

[File|Files] :-
	consult([File|Files]).




:- meta_predicate(consult(:)).
:- meta_predicate(consult(:, +)).

consult(File) :-
	'$consult'(File, [], 1).


consult(File, Options) :-
	'$consult'(File, Options, 2).


'$consult'(File, Options, Arity) :-
	set_bip_name(consult, Arity),
	'$check_atom_or_atom_list'(File),
	'$set_consult_defaults',
	'$get_consult_options'(Options, Pl2WamArgs1, Pl2WamArgs),
	'$add_args_for_flags'([bf(0 = 0, show_information = on, '--compile-msg'),
			       f(suspicious_warning = off, '--no-susp-warn'),
			       f(singleton_warning = off, '--no-singl-warn')], Pl2WamArgs1),
	(   atom(File), File \== [] ->
	    LFile = [File]
	;   LFile = File
	),
	member(File1, LFile),
	\+ '$consult1'(File1, Pl2WamArgs, Arity),
	!,
	fail.

'$consult'(_, _,_).




          % option mask in sys_var[0]:
          %
          %  b0
          %  0/1
          % quiet

'$set_consult_defaults' :-
	'$sys_var_write'(0, 0).




'$get_consult_options'(Options, Pl2WamArgsEnd, Pl2WamArgs) :-
	'$check_list'(Options),
	'$get_consult_options1'(Options, Pl2WamArgsEnd, Pl2WamArgs), !.



'$get_consult_options1'([], Pl2WamArgs, Pl2WamArgs).

'$get_consult_options1'([X|Options], Pl2WamArgsEnd, Pl2WamArgs) :-
	'$get_consult_options2'(X, Pl2WamArgs1, Pl2WamArgs), !,
	'$get_consult_options1'(Options, Pl2WamArgsEnd, Pl2WamArgs1).


'$get_consult_options2'(X, _, _) :-
	var(X),
	'$pl_err_instantiation'.

'$get_consult_options2'(quiet, Pl2WamArgs, Pl2WamArgs) :-
	'$sys_var_set_bit'(0, 0).

'$get_consult_options2'(pre_load(X), Pl2WamArgs, ['--pre-load', X|Pl2WamArgs]) :-
	'$check_nonvar'(X),
	atom(X).

'$get_consult_options2'(X, _, _) :-
	'$pl_err_domain'(consult_option, X).




'$add_args_for_flags'([], []).

'$add_args_for_flags'([bf(Bit = BValue, Flag = Value, Arg)|L], [Arg|Pl2WamArgs]) :-
	'$sys_var_get_bit'(0, Bit, BValue), 
	current_prolog_flag(Flag, Value), !,
	'$add_args_for_flags'(L, Pl2WamArgs).

'$add_args_for_flags'([f(Flag = Value, Arg)|L], [Arg|Pl2WamArgs]) :-
	current_prolog_flag(Flag, Value), !,
	'$add_args_for_flags'(L, Pl2WamArgs).

'$add_args_for_flags'([_|L], Pl2WamArgs) :-
	'$add_args_for_flags'(L, Pl2WamArgs).




'$consult1'(File, Pl2WamArgs1, Arity) :-
	'$call_c_test'('Pl_Prolog_File_Name_2'(File, File1)),
	(   File1 = user ->
	    File2 = File1
	;   '$call_c_test'('Pl_Absolute_File_Name_2'(File1, File2)),
	    (   file_exists(File2) ->
	        true
	    ;   set_bip_name(consult, Arity),
	        '$pl_err_existence'(source_sink, File1)
	    )
	),
	PipedConsult = piped,	% can be spawn, exec (Prolog) or piped (see consult_c.c)
	temporary_file('', gplc, TmpFile),
	atom_concat(TmpFile, '.wbc', TmpByteCodeFile),
	atom_concat(TmpFile, '.pl', TmpPreLoadFile),
	'$create_pre_load_file'(PipedConsult, TmpPreLoadFile),
	Pl2WamArgs = ['-w', File2, '--pre-load', TmpPreLoadFile, '-o', TmpByteCodeFile|Pl2WamArgs1],
%	Pl2WamArgs = ['-w', File2, '-o', TmpByteCodeFile|Pl2WamArgs1], % without pre-load (for debug)
	set_bip_name(consult, Arity),	
	(   '$consult2'(PipedConsult, Pl2WamArgs) ->
	    '$load_file'(TmpByteCodeFile),
	    unlink(TmpByteCodeFile),
	    unlink(TmpPreLoadFile)
	;   unlink(TmpByteCodeFile),
	    unlink(TmpPreLoadFile),
	    format(top_level_output, 'compilation failed~n', []),
	    fail
	).




'$create_pre_load_file'(piped, PreLoadFile) :-
	!,
	'$sys_var_read'(20, Say_GetC), 
	'$sys_var_write'(20, 1), % activate SAY_GETC in case of piped consult (see consult_c.c)
	'$write_pre_load_file'(PreLoadFile),
	'$sys_var_write'(20, Say_GetC).

'$create_pre_load_file'(_, PreLoadFile) :-
	'$write_pre_load_file'(PreLoadFile).




'$write_pre_load_file'(PreLoadFile) :-
	open(PreLoadFile, write, S, []),
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

'$write_pre_load_file'(_).


'$write_pre_load_goal'(S, Goal) :-
	portray_clause(S, (:- initialization(Goal))).




'$consult2'(spawn, Pl2WamArgs) :-
	spawn(pl2wam, Pl2WamArgs, 0).

'$consult2'(exec, Pl2WamArgs) :-
	'$list_to_atom'([pl2wam|Pl2WamArgs], Cmd),
	exec(Cmd, top_level_input, top_level_output, top_level_output, Pid),
	wait(Pid, 0).

'$consult2'(piped, Pl2WamArgs) :-
	'$call_c_test'('Pl_Consult_1'(Pl2WamArgs)).


'$list_to_atom'(List, Atom) :-
	open_output_atom_stream(S),
	(   member(A, List),
	    write(S, A),
	    put_char(S, ' '),
	    fail
	;   close_output_atom_stream(S, Atom)
	).



'$load_file'(BCFile) :-
	open(BCFile, read, Stream),
	repeat,
	read(Stream, P),
	(   P = end_of_file ->
	    !
	;   '$load_pred'(P, Stream),
	    fail
	),
	close(Stream).




'$load_pred'(file_name(PlFile), _) :-
	g_assign('$pl_file', PlFile).

'$load_pred'(directive(PlLine, Type, Goal), _) :-
	(   '$catch'(Goal, CallErr, '$load_directive_exception'(CallErr, PlLine, Type), load, 1, true) ->
	    true
	;   g_read('$pl_file', PlFile),
	    format(top_level_output, '~Nwarning: ~a:~d: ~a directive failed~n', [PlFile, PlLine, Type])
	).


'$load_pred'(predicate(PI, PlLine, StaDyn, PubPriv, MonoMulti, UsBplBfd, NbCl), Stream) :-
	PI = Pred / N,
	g_read('$pl_file', PlFile),
	'$check_pred_type'(Pred, N, PlFile, PlLine),
	(   MonoMulti = multifile, '$predicate_property1'(Pred, N, multifile) ->
	    true
	;
	    '$check_owner_files'(PI, PlFile, PlLine)
	),
	'$bc_start_pred'(Pred, N, PlFile, PlLine, StaDyn, PubPriv, MonoMulti, UsBplBfd),
	g_assign('$ctr', 0),
	repeat,
	g_read('$ctr', Ctr),
	Ctr1 is Ctr + 1,
	g_assign('$ctr', Ctr1),
	(   Ctr = NbCl ->
	    true
	;   read(Stream, clause(Cl, WamCl)),
	    '$add_clause_term_and_bc'(Cl, PlFile, WamCl),
	    fail
	), !.





'$load_directive_exception'(CallErr, PlLine, Type) :-
	g_read('$pl_file', PlFile),
	format(top_level_output, '~Nwarning: ~a:~d: ~a directive caused exception: ~q~n', [PlFile, PlLine, Type, CallErr]).




'$check_pred_type'(Pred, N, PlFile, PlLine) :-
	'$predicate_property1'(Pred, N, native_code), !,
	(   '$aux_name'(Pred) ->
	    true
	;   format(top_level_output, 'error: ~a:~d: native code procedure ~q cannot be redefined (ignored)~n', [PlFile, PlLine, Pred/N])
	),
	fail.

'$check_pred_type'(_, _, _, _).




'$check_owner_files'(PI, PlFile, PlLine) :-
	'$get_predicate_file_info'(PI, PlFile1, PlLine1),
	PlFile \== PlFile1, !,
	PI = Name / _,
	(   '$aux_name'(Name) ->
	    true
	;   format(top_level_output, 'warning: ~a:~d: redefining procedure ~q~n', [PlFile, PlLine, PI]),
	    format(top_level_output, '         ~a:~d: previous definition~n', [PlFile1, PlLine1])
	).

'$check_owner_files'(_, _, _).




load(File) :-
	set_bip_name(load, 1),
	'$check_atom_or_atom_list'(File), !,
	(   atom(File),
	    File \== [] ->
	    '$load2'(File)
	;   '$load1'(File)
	).


'$load1'([]).

'$load1'([File|Files]) :-
	'$load2'(File),
	'$load1'(Files).


'$load2'(File) :-
	decompose_file_name(File, _Dir, _Prefix, Suffix),
	(   Suffix = '' ->
	    atom_concat(File, '.wbc', File1)
	;   File1 = File
	),
	'$call_c_test'('Pl_Absolute_File_Name_2'(File1, File2)),
	(   file_exists(File2) ->
	    true
	;   set_bip_name(load, 1),
	    '$pl_err_existence'(source_sink, File1)
	),
	set_bip_name(load, 1),
	'$load_file'(File1).




'$bc_start_pred'(Pred, N, PlFile, PlLine, StaDyn, PubPriv, MonoMulti, UsBplBfd) :-
	'$call_c'('Pl_BC_Start_Pred_8'(Pred, N, PlFile, PlLine, StaDyn, PubPriv, MonoMulti, UsBplBfd)).


'$bc_start_emit' :-
	'$call_c'('Pl_BC_Start_Emit_0').

'$bc_stop_emit' :-
	'$call_c'('Pl_BC_Stop_Emit_0').

'$bc_emit'([]).

'$bc_emit'([WamInst|WamCode]) :-
	'$bc_emit_inst'(WamInst),
	'$bc_emit'(WamCode).

'$bc_emit_inst'(WamInst) :-
	'$call_c'('Pl_BC_Emit_Inst_1'(WamInst)).




'$bc_emulate_cont' :-                  % used by C code to set a continuation
	'$call_c_jump'('Pl_BC_Emulate_Cont_0').




'$add_clause_term'(Cl, PlFile) :-
	'$assert'(Cl, 0, 0, PlFile).




'$add_clause_term_and_bc'(Cl, PlFile, WamCl) :-
	'$bc_start_emit',
	'$bc_emit'(WamCl),
	'$bc_stop_emit',
	'$add_clause_term'(Cl, PlFile).




          % Listing

listing :-
	set_bip_name(listing, 0),
	'$sys_var_write'(5, 0),
	'$listing_all'(_).



:- meta_predicate(listing(:)).

listing(PI) :-
	set_bip_name(listing, 1),
	'$sys_var_write'(5, 0),
	var(PI), !,
	'$pl_err_instantiation'.

listing(N) :-
	atom(N), !,
	'$listing_all'(N / _).

listing(PI) :-
	'$listing_all'(PI).



	% same but also shows '$xxx' predicates

'$listing_any' :-
	set_bip_name('$listing_any', 0),
	'$sys_var_write'(5, 1),	
	'$listing_all'(_).



'$listing_any'(PI) :-
	set_bip_name('$listing_any', 1),
	'$sys_var_write'(5, 1),
	var(PI), !,
	'$pl_err_instantiation'.

'$listing_any'(N) :-
	atom(N), !,
	'$listing_all'(N / _).

'$listing_any'(PI) :-
	'$listing_all'(PI).






/* NEW version which orders the output by file then by line number */

'$listing_all'(PI) :-  % setof: for each File returns a sorted list [Line-PI,...]
	setof(Line-PI, '$listing_one_pi'(File, Line, PI), LKPI), 
	format('~n%% file: ~w~n', [File]),
	member(_-PI1, LKPI),
	'$listing_one'(PI1),
	fail.

'$listing_all'(_).


'$listing_one_pi'(File, Line, PI) :-
	(   '$sys_var_read'(5, 0) ->
	    '$current_predicate'(PI)
	;
	    '$current_predicate_any'(PI), PI = Pred / _, '$not_aux_name'(Pred)
	),
	\+ '$predicate_property_pi_any'(PI, native_code),
	'$predicate_property_pi_any'(PI, prolog_file(File)),
	'$predicate_property_pi_any'(PI, prolog_line(Line)).



/* OLD version which does not order the output
   
'$listing_all'(PI) :-
	current_prolog_flag(strict_iso, SI),
	(   set_prolog_flag(strict_iso, off),
	    '$current_predicate'(PI),
	    '$listing_one'(PI),
	    fail
	;   set_prolog_flag(strict_iso, SI)
	).

*/


'$listing_one'(PI) :-
	'$predicate_property_pi_any'(PI, native_code), !,
	true.

'$listing_one'(PI) :-
	'$get_pred_indic'(PI, N, A),
	functor(H, N, A),
	nl,
	set_bip_name(listing, 0), %only for debug of dynam_supp.c (see owner_func/arity if not passed)
	'$clause'(H, B, 2),
	portray_clause((H :- B)),
	fail.

'$listing_one'(_).
