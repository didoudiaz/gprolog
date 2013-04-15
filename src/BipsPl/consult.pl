/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : consult.pl                                                      *
 * Descr.: file consulting                                                 *
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

'$use_consult'.


[File|Files] :-
	consult([File|Files]).




consult(File) :-
	set_bip_name(consult, 1),
	'$check_atom_or_atom_list'(File), !,
	(   atom(File),
	    File \== [] ->
	    '$consult2'(File)
	;   '$consult1'(File)
	).


'$consult1'([]).

'$consult1'([File|Files]) :-
	'$consult2'(File),
	'$consult1'(Files).


'$consult2'(File) :-
	'$call_c_test'('Pl_Prolog_File_Name_2'(File, File1)),
	(   File1 = user ->
	    File2 = File1
	;   '$call_c_test'('Pl_Absolute_File_Name_2'(File1, File2)),
	    (   file_exists(File2) ->
	        true
	    ;   set_bip_name(consult, 1),
	        '$pl_err_existence'(source_sink, File1)
	    )
	),
	temporary_file('', gplc, TmpFile),
	set_bip_name(consult, 1),
	(   '$consult3'(TmpFile, File2) ->
	    '$load_file'(TmpFile),
	    unlink(TmpFile)
	;   unlink(TmpFile),
	    format(top_level_output, 'compilation failed~n', []),
	    fail
	).




'$consult3'(TmpFile, PlFile) :-
	'$call_c_test'('Pl_Consult_2'(TmpFile, PlFile)).

/*
'$consult3'(TmpFile,PlFile):-
	write_pl_state_file(TmpFile),
	Args=['-w','--compile-msg','--pl-state',TmpFile,'-o',TmpFile,PlFile
              |End],
        (current_prolog_flag(singleton_warning,on)
            -> End=[]
            ;  End=['--no-singl-warn']),
	spawn(pl2wam,Args,0).
*/




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
	'$listing_all'(_).



listing(PI) :-
	set_bip_name(listing, 1),
	var(PI), !,
	'$pl_err_instantiation'.

listing(N) :-
	atom(N), !,
	'$listing_all'(N / _).

listing(PI) :-
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
	'$current_predicate'(PI),
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
	'$clause'(H, B, 2),
	portray_clause((H :- B)),
	fail.

'$listing_one'(_).
