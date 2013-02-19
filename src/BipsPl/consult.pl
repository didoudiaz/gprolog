/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : consult.pl                                                      *
 * Descr.: file consulting                                                 *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2012 Daniel Diaz                                     *
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

/* $Id$ */

:-	built_in.

'$use_consult'.


	/* the meta_predicate declaration is to allow foo:consult(File)
	 * foo becomes the destination module. If File does not contain any module definition
	 * (i.e. module = user) then the consult is done in the destination module (foo) instead of user.
	 */

:- meta_predicate(consult(:)).
:- meta_predicate('.'(:, +)).

[File|Files] :-
	consult([File|Files]).


consult(MFile) :-
	set_bip_name(consult, 1),
	'$strip_module_nonvar'(MFile, Module, File), % the destination module
	'$check_atom_or_atom_list'(File), !,
	(   atom(File), File \== [] ->
	    '$consult2'(File, Module)
	;
	    '$consult1'(File, Module)
	).


'$consult1'([], _).

'$consult1'([File|Files], Module) :-
	'$consult2'(File, Module),
	'$consult1'(Files, Module).


'$consult2'(MFile, Module0) :-
	'$strip_module_nonvar'(MFile, Module1, File), % individual destination module
	(   var(Module1) ->			      % not given, use inherited destination module
	    Module = Module0
	;
	    Module = Module1			      % given: use this
	),
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
	    '$load_file'(TmpFile, Module),
	    unlink(TmpFile)
	;
	    unlink(TmpFile),
	    format(top_level_output, 'compilation failed~n', []),
	    fail
	).




'$consult3'(TmpFile, PlFile) :-
	'$call_c_test'('Pl_Consult_2'(TmpFile, PlFile)).

/*
'$consult3'(TmpFile,PlFile):-
	write_pl_state_file(TmpFile),
	Args=['-w','--compile-msg','--pl-state',TmpFile,'-o',TmpFile,PlFile|End],
        (current_prolog_flag(singleton_warning,on)
            -> End=[]
            ;  End=['--no-singl-warn']),
	spawn(pl2wam,Args,0).
*/




'$load_file'(BCFile, Module) :-	% Module is the destination Module
	open(BCFile, read, Stream),
	repeat,
	read(Stream, P),
	(   P = end_of_file ->
	    !
	;
	    '$load_pred'(P, Stream, Module),
	    fail
	),
	close(Stream).


/* DEBUG
'$load_pred'(P, _Stream, Module) :-
	format('LOAD: ~w   MOD:~w~n', [P, Module]),
	fail.
*/

'$load_pred'(file_name(PlFile), _, _) :-
	g_assign('$pl_file', PlFile).

'$load_pred'(directive(PlLine, Type, Goal), _, Module) :-
	(   '$catch'(Goal, CallErr, '$load_directive_exception'(CallErr, PlLine, Type), Module, load, 1) ->
	    true
	;
	    g_read('$pl_file', PlFile),
	    format(top_level_output, '~Nwarning: ~a:~d: ~a directive failed~n', [PlFile, PlLine, Type])
	).


'$load_pred'(predicate(PI, PlLine, StaDyn, PubPriv, MonoMulti, UsBplBfd, NbCl), Stream, Module0) :-
	'$load_pred_extract_module'(PI, Module0, Pred, N, Module), !,
	g_read('$pl_file', PlFile),
	'$check_pred_type'(Module, Pred, N, PlFile, PlLine),
% DEBUG format('LOAD: pred ~w:~w/~w  from:~w:~w~n', [Module, Pred, N, PlFile, PlLine]),
	(   MonoMulti = multifile, '$predicate_property1'(Module, Pred, N, multifile) ->
	    true
	;
	    '$check_owner_files'(Module, Pred, N, PlFile, PlLine)
	),
	'$bc_start_pred'(Module, Pred, N, PlFile, PlLine, StaDyn, PubPriv, MonoMulti, UsBplBfd),
	g_assign('$ctr', 0),
	repeat,
	g_read('$ctr', Ctr),
	Ctr1 is Ctr + 1,
	g_assign('$ctr', Ctr1),
	(   Ctr = NbCl ->
	    true
	;
	    read(Stream, clause(Cl, WamCl)),
	    '$add_clause_term_and_bc'(Module, Cl, PlFile, WamCl),
	    fail
	), !.


				% if user, select the destination Module
'$load_pred_extract_module'(user:Pred/N, Module, Pred, N, Module).

				% if pred module != user use it
'$load_pred_extract_module'(Module:Pred/N, _, Pred, N, Module).

				% if no module specified use destination Module (should not occur)
'$load_pred_extract_module'(Pred/N, Module, Pred, N, Module).




'$load_directive_exception'(CallErr, PlLine, Type) :-
	g_read('$pl_file', PlFile),
	format(top_level_output, '~Nwarning: ~a:~d: ~a directive caused exception: ~q~n', [PlFile, PlLine, Type, CallErr]).




'$check_pred_type'(Module, Pred, N, PlFile, PlLine) :-
	'$predicate_property1'(Module, Pred, N, native_code), !,
	(   '$aux_name'(Pred) ->
	    true
	;   format(top_level_output, 'error: ~a:~d: native code procedure ~q cannot be redefined (ignored)~n', [PlFile, PlLine, Pred/N])
	),
	fail.

'$check_pred_type'(_, _, _, _, _).




'$check_owner_files'(Module, F, N, PlFile, PlLine) :-
	'$get_predicate_file_info'(Module, F, N, PlFile1, PlLine1),
	PlFile \== PlFile1, !,
	(   '$aux_name'(F) ->
	    true
	;   format(top_level_output, 'warning: ~a:~d: redefining procedure ~q~n', [PlFile, PlLine, F/N]),
	    format(top_level_output, '         ~a:~d: previous definition~n', [PlFile1, PlLine1])
	).

'$check_owner_files'(_, _, _, _, _).



:- meta_predicate(load(:)).

load(MFile) :-
	set_bip_name(load, 1),
	'$strip_module_nonvar'(MFile, Module, File), % the destination module
	'$check_atom_or_atom_list'(File), !,
	(   atom(File),
	    File \== [] ->
	    '$load2'(File, Module)
	;   '$load1'(File, Module)
	).


'$load1'([], _).

'$load1'([File|Files], Module) :-
	'$load2'(File, Module),
	'$load1'(Files, Module).


'$load2'(MFile, Module0) :-
	'$strip_module_nonvar'(MFile, Module1, File), % individual destination module
	(   var(Module1) ->			      % not given, use inherited destination module
	    Module = Module0
	;
	    Module = Module1			      % given: use this
	),
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
	'$load_file'(File1, Module).




'$bc_start_pred'(Module, Pred, N, PlFile, PlLine, StaDyn, PubPriv, MonoMulti, UsBplBfd) :-
	'$call_c'('Pl_BC_Start_Pred_9'(Module, Pred, N, PlFile, PlLine, StaDyn, PubPriv, MonoMulti, UsBplBfd)).


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



%  called by compiled code for dynamic or multifile predicate

'$add_clause_term'(Module, Cl, PlFile) :-
	'$assert'(Module:Cl, 0, 0, PlFile).



% called by '$load_pred'

'$add_clause_term_and_bc'(Module, Cl, PlFile, WamCl) :-
	'$bc_start_emit',
	'$bc_emit'(WamCl),
	'$bc_stop_emit',
	'$add_clause_term'(Module, Cl, PlFile).




          % Listing

:- meta_predicate(listing(:)).

listing :-
	set_bip_name(listing, 0),
	'$listing_all'(user, _, _). %FIXME what to do with a meta_pred without argument ?



listing(MPI) :-
	set_bip_name(listing, 1),
	'$strip_module_var'(MPI, Module, PI),
	(   atom(PI) ->
	    Func = PI
	;
	    '$get_pred_indic'(PI, Module, _, Func, Arity)
	),
	'$listing_all'(Module, Func, Arity).

/* NEW version which orders the output by file then by line number */


'$listing_all'(Module, Func, Arity) :- % setof: for each File returns a sorted list [Line-PI,...]
	g_assign('$last_module', Module),
	setof(v(Line, Func, Arity), '$listing_one_collect'(Module, Func, Arity, File, Line), LP),
	g_read('$last_module', LastModule),
	(   LastModule == Module ->
	    true
	;
	    format('~n:- module(~q).~n', [Module]),
	    g_assign('$last_module', Module)
	),
	format('~n%% file: ~w~n', [File]),
	member(v(_, Func1, Arity1), LP),
	'$listing_one_show'(Module, Func1, Arity1). % this one fails

'$listing_all'(_, _, _).


'$listing_one_collect'(Module, Func, Arity, File, Line) :-
	'$current_predicate'(Module:Func/Arity),
   	(   '$call_c_test'('Pl_Pred_Prop_Native_Code_2'(Module, Func, Arity)) ->
	    fail
	;
	    '$call_c_test'('Pl_Pred_Prop_Prolog_File_3'(Module, Func, Arity, File)),
	    '$call_c_test'('Pl_Pred_Prop_Prolog_Line_3'(Module, Func, Arity, Line))
	).



/* OLD version which does not order the output
  
'$listing_all'(Module, Func, Arity) :-   
	g_assign('$last_module', Module),
	'$current_predicate'(Module:Func/Arity),
	(   '$call_c_test'('Pl_Pred_Prop_Native_Code_2'(Module, Func, Arity)) ->
	    fail
	;
	    true
	),
	g_read('$last_module', LastModule),
	(   LastModule == Module ->
	    true
	;
	    format(':- module(~q).~n', [Module]),
	    g_assign('$last_module', Module)
	),
	'$listing_one_show'(Module, Func, Arity). % this one fails
*/



'$listing_one_show'(Module, Func, Arity) :-
	functor(H, Func, Arity),
	nl,
	'$clause'(Module:H, B, false),
	portray_clause((H :- B)),
	fail.


