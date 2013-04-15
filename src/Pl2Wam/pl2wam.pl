/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog to WAM compiler                                          *
 * File  : pl2wam.pl                                                       *
 * Descr.: main file                                                       *
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


pl2wam(Arg) :-
	atom(Arg),
	Arg \== [], !,			% to call easily inder top-level
	pl2wam([Arg]).

pl2wam(LArg) :-
	catch(pl2wam1(LArg), Err, exception(Err)).




pl2wam1(LArg) :-
	cmd_line_args(LArg, PlFile, WamFile),
	prolog_file_name(PlFile, PlFile1),
	g_read(native_code, NativeCode),
	compile_msg_start(PlFile1, NativeCode),
	read_file_init(PlFile),
	emit_code_init(WamFile, PlFile),
	init_counters,
	repeat,
	read_predicate(Pred, N, LSrcCl),
	add_counter(user_read_file, real_read_file),
	(   LSrcCl = [] ->	% [] at end of file
	    !
	;
	    read_file_error_nb(0),
	    compile_and_emit_pred(NativeCode, Pred, N, LSrcCl),
	    fail
	),
	emit_ensure_linked,
	read_file_term(InBytes, InLines),
	emit_code_term(OutBytes, OutLines),
	read_file_error_nb(ErrNb),
	(   ErrNb = 0 ->
	    display_counters,
	    compile_msg_end(PlFile1, InBytes, InLines, OutBytes, OutLines)
	;   format('~N\t~d error(s)~n', [ErrNb]),
	    abort
	).




compile_and_emit_pred(t, Pred, N, LSrcCl) :-
	compile_emit_inits(Pred, N, LSrcCl, PlFile, PlLine),
	compile_lst_clause(LSrcCl, LCompCl),
	indexing(LCompCl, WamCode),
	add_counter(user_indexing, real_indexing),
	emit_code(Pred, N, PlFile, PlLine, WamCode),
	add_counter(user_wam_emit, real_wam_emit).

compile_and_emit_pred(f, Pred, N, LSrcCl) :-
	compile_emit_inits(Pred, N, LSrcCl, PlFile, PlLine),
	bc_compile_lst_clause(LSrcCl, LCompCl),
	bc_emit_code(Pred, N, PlFile, PlLine, LCompCl),
	add_counter(user_wam_emit, real_wam_emit).




compile_emit_inits(Pred, N, LSrcCl, PlFile1, PlLine) :-
	g_assign(cur_func, Pred),
	g_assign(cur_arity, N),
	syntactic_sugar_init_pred(Pred, N),
	LSrcCl = [[PlFile * _|_] + (PlLine - _) + _|_],
	absolute_file_name(PlFile, PlFile1).




compile_lst_clause([], []).

compile_lst_clause([SrcCl|LSrcCl], [cl(_, FirstArg, WamCl)|LCC]) :-
	compile_clause(SrcCl, FirstArg, WamCl),
	compile_lst_clause(LSrcCl, LCC).




compile_clause(Where + Cl, FirstArg, WamCl) :-
	g_assign(where, Where),
	syntactic_sugar(Cl, Head, Body),
	add_counter(user_syn_sugar, real_syn_sugar),
	internal_format(Head, Body, Head1, Body1, NbChunk, NbY),
	add_counter(user_internal, real_internal),
	code_generation(Head1, Body1, NbChunk, NbY, WamCl),
	add_counter(user_code_gen, real_code_gen),
	allocate_registers(WamCl),
	add_counter(user_reg_alloc, real_reg_alloc),
	find_first_arg(WamCl, FirstArg),
	add_counter(user_first_arg, real_first_arg).




bc_compile_lst_clause([], []).

bc_compile_lst_clause([SrcCl|LSrcCl], [bc(Cl, WamCl)|LCC]) :-
	SrcCl = _ + Cl,
	compile_clause(SrcCl, _FirstArg, WamCl),
	bc_compile_lst_clause(LSrcCl, LCC).



compile_msg_start(_, _) :-
	g_read(compile_msg, f), !.

compile_msg_start(PlFile, NativeCode) :-
	(   NativeCode = t ->
	    Type = 'native code'
	;   Type = 'byte code'
	),
	format('compiling ~a for ~a...~n', [PlFile, Type]),
	flush_output.




compile_msg_end(_, _, _, _, _) :-
	g_read(compile_msg, f), !.

compile_msg_end(PlFile, _InBytes, InLines, OutBytes, _OutLines) :-
	real_time(Time),
	format('~a compiled, ~d lines read - ~d bytes written, ~d ms~n', [PlFile, InLines, OutBytes, Time]).




cur_pred(Func, Arity) :-
	g_read(cur_func, Func),
	g_read(cur_arity, Arity).


cur_pred_without_aux(Func1, Arity1) :-
	cur_pred(Func, Arity),
	'$pred_without_aux'(Func, Arity, Func1, Arity1).




init_counters :-
	g_read(statistics, f), !.

init_counters :-
	g_assign(user_read_file, 0),
	g_assign(real_read_file, 0),
	g_assign(user_syn_sugar, 0),
	g_assign(real_syn_sugar, 0),
	g_assign(user_internal, 0),
	g_assign(real_internal, 0),
	g_assign(user_code_gen, 0),
	g_assign(real_code_gen, 0),
	g_assign(user_reg_alloc, 0),
	g_assign(real_reg_alloc, 0),
	g_assign(user_indexing, 0),
	g_assign(real_indexing, 0),
	g_assign(user_first_arg, 0),
	g_assign(real_first_arg, 0),
	g_assign(user_wam_emit, 0),
	g_assign(real_wam_emit, 0),
	last_times(_, _).




add_counter(_, _) :-
	g_read(statistics, f), !.

add_counter(UserCounter, RealCounter) :-
	last_times(User1, Real1),
	g_read(UserCounter, User2),
	g_read(RealCounter, Real2),
	User is User1 + User2,
	Real is Real1 + Real2,
	g_assign(UserCounter, User),
	g_assign(RealCounter, Real).




last_times(User, Real) :-
	statistics(real_time, [_, Real]),
	statistics(runtime, [_, User]).




display_counters :-
	g_read(statistics, f), !.

display_counters :-
	g_read(user_read_file, UReadFile),
	g_read(real_read_file, RReadFile),
	g_read(user_syn_sugar, USynSugar),
	g_read(real_syn_sugar, RSynSugar),
	g_read(user_internal, UInternal),
	g_read(real_internal, RInternal),
	g_read(user_code_gen, UCodeGen),
	g_read(real_code_gen, RCodeGen),
	g_read(user_reg_alloc, URegAlloc),
	g_read(real_reg_alloc, RRegAlloc),
	g_read(user_indexing, UIndexing),
	g_read(real_indexing, RIndexing),
	g_read(user_first_arg, UFirstArg),
	g_read(real_first_arg, RFirstArg),
	g_read(user_wam_emit, UWamEmit),
	g_read(real_wam_emit, RWamEmit),
	U is UReadFile + USynSugar + UInternal + UCodeGen + URegAlloc + UIndexing + UIndexing + UWamEmit,
	R is RReadFile + RSynSugar + RInternal + RCodeGen + RRegAlloc + RIndexing + RIndexing + RWamEmit,
	user_time(UTotal),
	real_time(RTotal),
	UMisc is UTotal - U,
	RMisc is RTotal - R,
	format('   Statistics (in ms)     user     real~n', []),
	format('   source reading     : %6d   %6d~n', [UReadFile, RReadFile]),
	format('   syntactic sugar    : %6d   %6d~n', [USynSugar, RSynSugar]),
	format('   internal format    : %6d   %6d~n', [UInternal, RInternal]),
	format('   code generation    : %6d   %6d~n', [UCodeGen, RCodeGen]),
	format('   register allocation: %6d   %6d~n', [URegAlloc, RRegAlloc]),
	format('   indexing           : %6d   %6d~n', [UIndexing, RIndexing]),
	format('   first arg computing: %6d   %6d~n', [UFirstArg, RFirstArg]),
	format('   code emission      : %6d   %6d~n', [UWamEmit, RWamEmit]),
	format('   other              : %6d   %6d~n', [UMisc, RMisc]),
	format('                Total : %6d   %6d~n', [UTotal, RTotal]).




          % Command-line options reading

cmd_line_args(LArg, PlFile, WamFile) :-
	g_assign(plfile, ''),
	g_assign(wamfile, ''),
	g_assign(native_code, t),
	g_assign(wam_comment, ''),
	g_assign(susp_warn, t),
	g_assign(singl_warn, t),
	g_assign(redef_error, t),
	g_assign(foreign_only, f),
	g_assign(call_c, t),
	g_assign(inline, t),
	g_assign(reorder, t),
	g_assign(reg_opt, 2),
	g_assign(opt_last_subterm, t),
	g_assign(keep_void_inst, f),
	g_assign(fast_math, f),
	g_assign(statistics, f),
	g_assign(compile_msg, f),
	cmd_line_args(LArg),
	g_read(plfile, PlFile),
	(   PlFile = '' ->
	    format('no input file~n', []),
	    abort
	;   true
	),
	g_read(wamfile, WamFile).




cmd_line_args([]).

cmd_line_args([Arg|LArg]) :-
	cmd_line_arg1(Arg, LArg, LArg1), !,
	cmd_line_args(LArg1).


cmd_line_arg1('-o', LArg, LArg1) :-
	cmd_line_arg1('--output', LArg, LArg1).

cmd_line_arg1('--output', LArg, LArg1) :-
	(   LArg = [WamFile|LArg1],
	    sub_atom(WamFile, 0, 1, _, Prefix),
	    Prefix \== (-)
	;   format('FILE missing after --output option~n', []),
	    abort
	),
	g_read(wamfile, WamFile0),
	(   WamFile0 = '' ->
	    true
	;   format('output file already specified (~a)~n', [WamFile0]),
	    abort
	),
	g_assign(wamfile, WamFile).

cmd_line_arg1('--pl-state', [File|LArg], LArg) :-
	read_pl_state_file(File),
	(   current_prolog_flag(singleton_warning, off) ->
	    g_assign(singl_warn, f)
	;   true
	).

cmd_line_arg1('-W', LArg, LArg1) :-
	cmd_line_arg1('--wam-for-native', LArg, LArg1).

cmd_line_arg1('--wam-for-native', LArg, LArg) :-
	g_assign(native_code, t).

cmd_line_arg1('-w', LArg, LArg1) :-
	cmd_line_arg1('--wam-for-byte-code', LArg, LArg1).

cmd_line_arg1('--wam-for-byte-code', LArg, LArg) :-
	g_assign(native_code, f),
	g_assign(inline, f),                              % force --no-inline
	g_assign(call_c, f).                              % force --no-call-c

cmd_line_arg1('--wam-comment', [Cmt|LArg], LArg) :-
	g_assign(wam_comment, Cmt).

cmd_line_arg1('--no-susp-warn', LArg, LArg) :-
	g_assign(susp_warn, f).

cmd_line_arg1('--no-singl-warn', LArg, LArg) :-
	g_assign(singl_warn, f).

cmd_line_arg1('--no-redef-error', LArg, LArg) :-
	g_assign(redef_error, f).

cmd_line_arg1('--foreign-only', LArg, LArg) :-
	g_assign(foreign_only, t).

cmd_line_arg1('--no-call-c', LArg, LArg) :-
	g_assign(call_c, f).

cmd_line_arg1('--no-inline', LArg, LArg) :-
	g_assign(inline, f).

cmd_line_arg1('--no-reorder', LArg, LArg) :-
	g_assign(reorder, f).

cmd_line_arg1('--no-reg-opt', LArg, LArg) :-
	g_assign(reg_opt, 0).

cmd_line_arg1('--min-reg-opt', LArg, LArg) :-
	g_assign(reg_opt, 1).

cmd_line_arg1('--no-opt-last-subterm', LArg, LArg) :-
	g_assign(opt_last_subterm, f).

cmd_line_arg1('--fast-math', LArg, LArg) :-
	g_assign(fast_math, t).

cmd_line_arg1('--keep-void-inst', LArg, LArg) :-
	g_assign(keep_void_inst, t).

cmd_line_arg1('--statistics', LArg, LArg) :-
	g_assign(statistics, t).

cmd_line_arg1('--compile-msg', LArg, LArg) :-
	g_assign(compile_msg, t).

cmd_line_arg1('--version', LArg, LArg) :-
	display_copying,
	stop.

cmd_line_arg1('-h', LArg, LArg1) :-
	cmd_line_arg1('--help', LArg, LArg1).

cmd_line_arg1('--help', LArg, LArg) :-
	(   h(L),
	    write(L),
	    nl,
	    fail
	;   nl,
	    write('Report bugs to bug-prolog@gnu.org.'),
	    nl,
	    stop
	).

cmd_line_arg1(Arg, _, _) :-
	sub_atom(Arg, 0, 1, _, -),
	format('unknown option ~a - try pl2wam --help~n', [Arg]),
	abort.

cmd_line_arg1(PlFile, LArg, LArg) :-
	g_read(plfile, PlFile0),
	(   PlFile0 = '' ->
	    true
	;   format('input file already specified (~a)~n', [PlFile0]),
	    abort
	),
	g_assign(plfile, PlFile).




          % Copying

display_copying :-
	prolog_name(Name),
	prolog_version(Version),
	prolog_copyright(Copyright),
	format('Prolog to Wam Compiler (~a) ~a~n', [Name, Version]),
	format('By Daniel Diaz~n', []),
	write(Copyright),
	nl,
	format('~a comes with ABSOLUTELY NO WARRANTY.~n', [Name]),
	format('You may redistribute copies of ~a~n', [Name]),
	format('under the terms of the GNU Lesser General Public License~n', []),
	format('or of the terms of the GNU General Public License (or both in parallel)~n', []),
	format('For more information about these matters, see the files named COPYING.~n', []).




prolog_name(Name) :-
	current_prolog_flag(prolog_name, Name).

prolog_version(Version) :-
	current_prolog_flag(prolog_version, Version).

prolog_date(Date) :-
	current_prolog_flag(prolog_date, Date).

prolog_copyright(Copyright) :-
	current_prolog_flag(prolog_copyright, Copyright).




          % Help

h('Usage: pl2wam [OPTION...] FILE').
h('').
h('Options:').
h('  -o FILE, --output FILE      set output file name').
h('  -W, --wam-for-native        produce a WAM file for native code').
h('  -w, --wam-for-byte-code     produce a WAM file for byte-code (force --no-call-c)').
h('  --pl-state FILE             read FILE to set the initial Prolog state').
h('  --wam-comment COMMENT       emit COMMENT as a comment in the WAM file').
h('  --no-susp-warn              do not show warnings for suspicious predicates').
h('  --no-singl-warn             do not show warnings for named singleton variables').
h('  --no-redef-error            do not show errors for built-in redefinitions').
h('  --foreign-only              only compile foreign/1-2 directives').
h('  --no-call-c                 do not allow the use of fd_tell, ''$call_c'',...').
h('  --no-inline                 do not inline predicates').
h('  --no-reorder                do not reorder predicate arguments').
h('  --no-reg-opt                do not optimize registers').
h('  --min-reg-opt               minimally optimize registers').
h('  --no-opt-last-subterm       do not optimize last subterm compilation').
h('  --fast-math                 fast mathematical mode (assume integer arithmetics)').
h('  --keep-void-inst            keep void instructions in the output file').
h('  --compile-msg               print a compile message').
h('  --statistics                print statistics information').
h('  --help                      print this help and exit').
h('  --version                   print version number and exit').
h('').
h('''user'' can be given as FILE for the standard input/output').




          % Starting directive

go :-
	argument_list(LArg),
	pl2wam(LArg).

:-	initialization(go).
