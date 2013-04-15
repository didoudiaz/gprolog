/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog to WAM compiler                                          *
 * File  : wam_emit.pl                                                     *
 * Descr.: code emission                                                   *
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


/*-------------------------------------------------------------------------*
 * WAM Instructions                                                        *
 *                                                                         *
 * get_variable(V, A)                       put_variable(V, A)             *
 *                                          put_void(A)                    *
 * get_value(V, A)                          put_value(V, A)                *
 *                                          put_unsafe_value(y(Y), A)      *
 * get_atom(F, A)                           put_atom(F, A)                 *
 * get_integer(N, A)                        put_integer(N, A)              *
 * get_float(D, A)                          put_float(D, A)                *
 * get_nil(A)                               put_nil(A)                     *
 * get_list(A)                              put_list(A)                    *
 * get_structure(F/N, A)                    put_structure(F/N, A)          *
 *                                          hide_meta_term(A)              *
 *                                                                         *
 *                                          math_load_value(V, A)          *
 *                                          math_fast_load_value(V, A)     *
 *                                                                         *
 * unify_variable(V)                        allocate(N)                    *
 * unify_void(N)                            deallocate                     *
 * unify_value(V)                                                          *
 * unify_local_value(V)                     call(F/N)                      *
 * unify_atom(F)                            execute(F/N)                   *
 * unify_integer(N)                         proceed                        *
 * unify_nil                                fail                           *
 * unify_list           (only for the last subterm if it is a list)        *
 * unify_structure(F/N) (only for the last subterm if it is a structure)   *
 *                                                                         *
 * label(L)                                                                *
 *                                                                         *
 * switch_on_term(Lvar, Latm, Lint, Llst, Lstc)                            *
 * switch_on_atom([(F,L),...])                                             *
 * switch_on_integer([(N,L),...])                                          *
 * switch_on_structure([(F/N,L),...])                                      *
 *                                                                         *
 * try_me_else(L)                           try(L)                         *
 * retry_me_else(L)                         retry(L)                       *
 * trust_me_else_fail                       trust(L)                       *
 *                                                                         *
 * get_current_choice(V)                    pragma_arity(N) (for cut)      *
 * cut(V)                                                                  *
 * soft_cut(V)                                                             *
 *                                                                         *
 * call_c(F, [T,...], [W,...])                                             *
 *   F=FctName, T=option only these options are relevant:                  *
 *    - jump/boolean/x(X) (jump at / test / move returned value)           *
 *    - set_cp (set CP before the call at the next instruction)            *
 *    - fast_call (use a fact call convention)                             *
 *    - tagged (use tagged calls for atoms, integers and F/N)              *
 *                                                                         *
 * foreign_call_c(F, T0, P/N, K, [(M1, T1),...])                           *
 *   F=FctName, T0=Return, P/N=BipName/BipArity, K=ChcSize                 *
 *   Mi=mode (in/out/in_out), Ti=type                                      *
 *                                                                         *
 * V      : x(X) or y(Y)                                                   *
 * X, Y   : integer >= 0                                                   *
 * A      : integer                                                        *
 * D      : float                                                          *
 * N, K   : integer                                                        *
 * F, T, M: atom                                                           *
 * W      : atom or integer or float or atom/integer or x(X)               *
 * L      : integer >= 1 (with no "holes") or 'fail' inside switch_on_term *
 *-------------------------------------------------------------------------*/

emit_code_init(WamFile0, PlFile0) :-
	prolog_file_name(PlFile0, PlFile),
	emit_code_files(WamFile0, PlFile, WamFile),
	(   WamFile = user ->
	    current_output(Stream)
	;   open(WamFile, write, Stream)
	),
	g_assign(streamwamfile, Stream),
	g_assign(cur_pl_file, ''),
	prolog_name(Name),
	prolog_version(Version),
	format(Stream, '%% compiler: ~a ~a~n', [Name, Version]),
	format(Stream, '%% file    : ~a~n', [PlFile]),
	g_read(wam_comment, Cmt),
	(   Cmt = '' ->
	    true
	;
	    format(Stream, '%%           ~a~n', [Cmt])
	).




emit_code_files('', user, user) :-
	!.

emit_code_files('', PlFile, WamFile) :-
	!,
	decompose_file_name(PlFile, _, Prefix, Suffix),
	(   g_read(native_code, t) ->
	    WamSuffix = '.wam'
	;   WamSuffix = '.wbc'
	),
	(   Suffix = '.pl' ->
	    atom_concat(Prefix, WamSuffix, WamFile)
	;   atom_concat(Prefix, Suffix, WF),
	    atom_concat(WF, WamSuffix, WamFile)
	).

emit_code_files(WamFile, _, WamFile).




emit_code_term(Bytes, Lines) :-
	g_read(streamwamfile, Stream),
	character_count(Stream, Bytes),
	line_count(Stream, Lines),
	close(Stream).




emit_code(Pred, N, PlFile, PlLine, WamCode) :-
	g_read(streamwamfile, Stream),
	emit_pred_start(Pred, N, PlFile, PlLine, Stream, _),
	emit_wam_code(WamCode, _, Stream),
	write(Stream, ']).'),
	nl(Stream).




emit_pred_start(Pred, 0, PlFile, PlLine, Stream, Type) :-
	(   Pred = '$exe_user',
	    Type = user
	;   Pred = '$exe_system',
	    Type = system
	), !,
	emit_file_name_if_needed(PlFile, Stream),
	format(Stream, '~n~ndirective(~d,~a,', [PlLine, Type]).

emit_pred_start(Pred, N, PlFile, PlLine, Stream, _) :-
	emit_file_name_if_needed(PlFile, Stream),
	(   test_pred_info(dyn, Pred, N) ->
	    StaDyn = dynamic
	;   StaDyn = static
	),
	(   test_pred_info(pub, Pred, N) ->
	    PubPriv = public
	;   PubPriv = private
	),
	(   test_pred_info(multi, Pred, N) ->
	    MonoMulti = multifile
	;   MonoMulti = monofile
	),
	g_read(module, Module0),
	export_type(Pred, N, Module0, _Module, ExportBplBfd),
  		% MODULES: then add Module:Pred/N instead of Pred/N in the next line
	format(Stream, '~n~npredicate(~q,~d,~a,~a,~a,~a,',
	       [Pred/N, PlLine, StaDyn, PubPriv, MonoMulti, ExportBplBfd]).



export_type(Pred, _, Module, Module, local) :-
	'$aux_name'(Pred), !.

export_type(Pred, N, Module, Module, local) :-
	test_pred_info(multi, Pred, N), !.

export_type(Pred, N, _, system, built_in) :-
	test_pred_info(bpl, Pred, N), !.

export_type(Pred, N, _, system, built_in_fd) :-
	test_pred_info(bfd, Pred, N), !.

export_type(Pred, N, system, system, built_in) :-  % an exported pred in system is a built_in - remove if wanted
	is_exported(Pred, N), !.

export_type(_, _, Module, Module, global) :-
	g_read(module_already_seen, f), !.

export_type(Pred, N, Module, Module, global) :-
	is_exported(Pred, N), !.

export_type(_, _, Module, Module, local).




emit_file_name_if_needed(PlFile, _) :-
	g_read(cur_pl_file, PlFile), !.

emit_file_name_if_needed(PlFile, Stream) :-
	format(Stream, '~n~nfile_name(~q).~n', [PlFile]),
	g_assign(cur_pl_file, PlFile).




emit_wam_code([], _, _).

emit_wam_code([WamInst|WamCode], First, Stream) :-
	emit_wam_code(WamInst, First, Stream),              % for nested code
	emit_wam_code(WamCode, First, Stream), !.

emit_wam_code(WamInst, First, Stream) :-
	special_form(WamInst, WamInst1),
	emit_wam_code(WamInst1, First, Stream).

emit_wam_code(WamInst, _, _) :-
	g_read(keep_void_inst, KeepVoidInst),
	dummy_instruction(WamInst, KeepVoidInst), !.

emit_wam_code(WamInst, First, Stream) :-
	WamInst = label(_), !,
	(   var(First) ->
	    Car = '[',
	    First = f
	;   Car = (',')
	),
	format(Stream, '~a~n~n', [Car]),
	emit_one_inst(WamInst, Stream).

emit_wam_code(WamInst, First, Stream) :-
	(   var(First) ->
	    Car = '[',
	    First = f
	;   Car = (',')
	),
	format(Stream, '~a~n    ', [Car]),
	emit_one_inst(WamInst, Stream).




emit_one_inst(WamInst, Stream) :-
	atom(WamInst), !,
	writeq(Stream, WamInst).

emit_one_inst(WamInst, Stream) :-
	functor(WamInst, F, N),
	writeq(Stream, F),
	emit_args(0, N, WamInst, Stream),
	write(Stream, ')').

emit_args(N, N, _, _) :-
	!.

emit_args(I, N, WamInst, Stream) :-
	I1 is I + 1,
	(   I1 = 1 ->
	    put_char(Stream, '(')
	;   put_char(Stream, ',')
	),
	arg(I1, WamInst, A),
	emit_one_arg(A, Stream),
	emit_args(I1, N, WamInst, Stream).

emit_one_arg([X|L], Stream) :-
	length(L, N),		% split long lists
	N > 30,
	!,
	put_char(Stream, '['),
	line_position(Stream, P),
	write_term(Stream, X, [quoted(true), priority(999)]),
	emit_list(L, P, Stream).

emit_one_arg(A, Stream) :-
	g_read(native_code, f),	    % if also wanted to .wam remove this line
	emit_one_f_n(A, Stream), !. % if fail breakthrough

emit_one_arg(A, Stream) :-
	writeq(Stream, A).


 /* this is added to fix a bug with consult/1. Pb with operators:
  *    :- op(500, xfx, edge).
  *    p(a edge b).
  *    :- op(0, xfx, edge).
  * will fail if edge is declared in the top-level before consult
  */

emit_one_f_n(M:P/N, Stream) :-
	format(Stream, '(~q):(~q)/~q', [M, P, N]).

emit_one_f_n(F/N, Stream) :-
	format(Stream, '(~q)/~q', [F, N]).




emit_list([], _, Stream) :-
	put_char(Stream, ']').

emit_list([X|L], P, Stream) :-
	format(Stream, ',~n~*c', [P, 32]), % changed 0' to 32 for emacs highlighting
	write_term(Stream, X, [quoted(true), priority(999)]),
	emit_list(L, P, Stream).




emit_ensure_linked :-
	g_read(streamwamfile, Stream),
	retract(ensure_linked(Name, Arity)), !,
	format(Stream, '~n~nensure_linked([~q', [Name / Arity]),
	(   clause(ensure_linked(Name1, Arity1), _),
	    format(Stream, ',~q', [Name1 / Arity1]),
	    fail
	;   true
	),
	write(Stream, ']).'),
	nl(Stream).

emit_ensure_linked.




bc_emit_code(Pred, N, PlFile, PlLine, LCompCl) :-
	g_read(streamwamfile, Stream),
	emit_pred_start(Pred, N, PlFile, PlLine, Stream, Type),
	(   nonvar(Type) ->
	    LCompCl = [bc((_ :- Body), _)],
	    bc_emit_prolog_term(Stream, Body),
	    format(Stream, ').~n', [])
	;   length(LCompCl, NbCl),
	    (   LCompCl = [bc('$$empty$$predicate$$clause$$', [proceed])] ->
	        NbCl1 = 0,
	        LCompCl1 = []
	    ;   NbCl1 = NbCl,
	        LCompCl1 = LCompCl
	    ),
	    format(Stream, '~d).~n', [NbCl1]),
	    bc_emit_lst_clause(LCompCl1, Stream)
	).




bc_emit_lst_clause([], _).

bc_emit_lst_clause([bc(Cl, WamCode)|LCompCl], Stream) :-
	format(Stream, '~n~nclause(', []),
	bc_emit_prolog_term(Stream, Cl),
	write(Stream, ','),
	emit_wam_code(WamCode, _, Stream),
	format(Stream, ']).~n', []),
	bc_emit_lst_clause(LCompCl, Stream).




bc_emit_prolog_term(Stream, Term) :- % create choice point for '$above'/1 write option
	'$get_current_B'(B),
	name_singleton_vars(Term),
	bind_variables(Term, [exclude([Term])]),
	write_term(Stream, Term, [numbervars(true), namevars(true), '$above'(B), ignore_ops(true), quoted(true)]),
	fail.

bc_emit_prolog_term(_, _).


