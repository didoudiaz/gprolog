/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog to WAM compiler                                          *
 * File  : read_file.pl                                                    *
 * Descr.: source file reading                                             *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2001 Daniel Diaz                                     *
 *                                                                         *
 * GNU Prolog is free software; you can redistribute it and/or modify it   *
 * under the terms of the GNU General Public License as published by the   *
 * Free Software Foundation; either version 2, or any later version.       *
 *                                                                         *
 * GNU Prolog is distributed in the hope that it will be useful, but       *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        *
 * General Public License for more details.                                *
 *                                                                         *
 * You should have received a copy of the GNU General Public License along *
 * with this program; if not, write to the Free Software Foundation, Inc.  *
 * 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     *
 *-------------------------------------------------------------------------*/

/* $Id$ */

/*-------------------------------------------------------------------------*
 * Data structures:                                                        *
 *                                                                         *
 * the stack of opened files (for nested includes):                        *
 *    global variable open_file_stack = [PlFile*Stream,...]                *
 *    from last to first.                                                  *
 *                                                                         *
 * the context (where occurs an error):                                    *
 *    global variable where = OpenFileStack+(L1-L2)                        *
 *    L1 = first line of the current clause (resp. directive).             *
 *    L2 = last  line of the current clause (resp. directive).             *
 *                                                                         *
 * read_predicate(Pred,N,LSrcCl):                                          *
 *    the structure of the compiler is a repeat/fail loop on 1 predicate   *
 *    calling read_predicate(Pred,N,LSrcCl) to obtain next predicate.      *
 *    Pred   = predicate name (an atom).                                   *
 *    N      = arity (an integer >=0).                                     *
 *    LSrcCl = [SrcCl,...], list of source clauses, with                   *
 *     SrcCl = Where+Cl where Cl is the source clause read.                *
 *                                                                         *
 * Buffers for special predicate management (with assert/retract):         *
 *                                                                         *
 * buff_aux_pred(Pred,N,LSrcCl):                                           *
 *    records the clauses of an auxiliary predicate.                       *
 *    Asserted by Pass 1 (syntactic sugar removing) when splitting ;/2,etc.*
 *    Retracted at the very next invocation of read_predicate/3 to         *
 *    ensure that aux. predicates always follow their "father" predicate.  *
 *                                                                         *
 * buff_discontig_clause(Pred,N,SrcCl):                                    *
 *    records a clause of a discontiguous predicate (:- discontiguous).    *
 *    Eacho clause of a discontiguous predicate is asserted when it is read*
 *    When the end of file is reached all clauses of a discontiguous pred  *
 *    are grouped to return a list of source clauses LSrcCl.               *
 *    Thus discontiguous predicates are always compiled after other        *
 *    predicates.                                                          *
 *                                                                         *
 * buff_dyn_interf_clause(Pred,N,SrcCl):                                   *
 *    records the interface clause of a dynamic predicate (:- dynamic).    *
 *    This clause is of the form Head:- call(Head) and only ensures that   *
 *    an external invocation to this predicate will not need to know that  *
 *    it is dynamic (and should be called by call/1).                      *
 *    Asserted as soon as a :- dynamic directive is encountered.           *
 *    Retracted only when the end of file is reached. All other clauses of *
 *    a dynamic predicate give rise to a system executable directive to    *
 *    assert(z) it.                                                        *
 *                                                                         *
 * empty_dyn_pred(Pred,N,Where):                                           *
 *    asserted when the declaration of a dynamic predicate is encoutered   *
 *    (:- dynamic). Retracted when a clause of a dynamic predicate is read.*
 *    At the end, only contains dynamic predicate with no clauses.         *
 *    Used then to define the interface clause.                            *
 *                                                                         *
 * ensure_linked(Pred,N):                                                  *
 *    asserted for each Pred/N occuring in a :- ensure_linked directive.   *
 *                                                                         *
 * Buffers for executable directive management (with assert/retract):      *
 *                                                                         *
 * buff_exe_system(SrcDirec)                                               *
 *     SrcDirec = Where+Body (i.e. source directive).                      *
 *     records a system directive.                                         *
 *     Asserted for dynamic clauses (to assertz it), and to execute, at    *
 *     run-time, op/3 set_prolog_flag/2, char_conversion/2.                *
 *     Retracted only when the end of file has been reached to provide a   *
 *     predicate '$exe_system':- Body.                                     *
 *                                                                         *
 * buff_exe_user(SrcDirec)                                                 *
 *     records user defined directives (:- initialization).                *
 *     Asserted when a :- initialization declaration is encountered.       *
 *     Retracted just after all '$buff_exe_system' to ensure that any user *
 *     directive has the needed environment.                               *
 *                                                                         *
 * Buffers for special clause management (with assert/retract):            *
 *                                                                         *
 * buff_clause(Pred,N,SrcCl):                                              *
 *    the reader needs a lookahead clause (to group clauses by predicates).*
 *    For such a clause we assert/retract(buff_clause(Pred,N,SrcCl)).      *
 *    Read at the very next invocation of get_next_clause/3.               *
 *-------------------------------------------------------------------------*/

:-	op(200, fx, ?).

read_file_init(PlFile) :-
	retractall(buff_clause(_, _, _)),
	retractall(buff_aux_pred(_, _, _)),
	retractall(buff_discontig_clause(_, _, _)),
	retractall(buff_dyn_interf_clause(_, _, _)),
	retractall(buff_exe_system(_)),
	retractall(buff_exe_user(_)),
	retractall(empty_dyn_pred(_, _, _)),
	retractall(ensure_linked(_, _)),
	retractall(pred_info(_, _, _)),
	g_assign(default_kind, user),
	g_assign(reading_dyn_pred, f),
	g_assign(eof_reached, f),
	g_assign(open_file_stack, []),
	g_assign(where, 0),
	g_assign(syn_error_nb, 0),
	g_assign(in_lines, 0),
	g_assign(in_bytes, 0),
	open_new_prolog_file(PlFile).




read_file_term(Bytes, Lines) :-
	g_read(in_bytes, Bytes),
	g_read(in_lines, Lines).




read_file_error_nb(SynErrNb) :-
	g_read(syn_error_nb, SynErrNb).




open_new_prolog_file(PlFile0) :-
	prolog_file_name(PlFile0, PlFile),
	(   PlFile = user ->
	    current_input(Stream)
	;   open(PlFile, read, Stream)
	),
	g_read(open_file_stack, OpenFileStack),
	g_assign(open_file_stack, [PlFile * Stream|OpenFileStack]).




close_last_prolog_file :-
	g_read(open_file_stack, [_ * Stream|OpenFileStack]),
	g_assign(open_file_stack, OpenFileStack),
	g_read(in_bytes, Bytes1),
	g_read(in_lines, Lines1),
	character_count(Stream, Bytes2),
	line_count(Stream, Lines2),
	Bytes is Bytes1 + Bytes2,
	Lines is Lines1 + Lines2,
	g_assign(in_bytes, Bytes),
	g_assign(in_lines, Lines),
	close(Stream).




          /*------------------*
           * Read a predicate *
           *------------------*/

read_predicate(Pred, N, LSrcCl) :-
	repeat,
	read_predicate1(Pred, N, LSrcCl),                % standard predicate
% !,
	(   g_read(reading_dyn_pred, f),
	    g_read(native_code, t) ->
	    read_predicate_next(Pred, N, LSrcCl)
	;   true
	).




read_predicate_next(Pred, N, LSrcCl) :-
	test_pred_info(dyn, Pred, N), !,
	LSrcCl = [Where + _|_],
	add_dyn_interf_clause(Pred, N, Where),
	create_exe_clauses_for_dyn_pred(LSrcCl, Pred, N),
	fail.                              % backtrack to repeat of main loop

read_predicate_next(Pred, N, LSrcCl) :-
	test_pred_info(pub, Pred, N), !,
	create_exe_clauses_for_pub_pred(LSrcCl).

read_predicate_next(_, _, _).




read_predicate1(Pred, N, LSrcCl) :-
	retract(buff_aux_pred(Pred, N, LSrcCl)), !.
                                                   % aux. pred (cf syn_sugar)

read_predicate1(Pred, N, LSrcCl) :-
	g_read(eof_reached, f), !,
	repeat,
	get_next_clause(Pred, N, SrcCl),
	SrcCl = _ + Cl,
	retractall(empty_dyn_pred(Pred, N, _)),
	(   test_pred_info(discontig, Pred, N) ->
	    assertz(buff_discontig_clause(Pred, N, SrcCl)),
	    define_predicate(Pred, N),
	    fail                               % backtrack to read_predicate1
	;   true
	),
	(   test_pred_info(def, Pred, N) ->
	    warn('discontiguous predicate ~q - clause ignored', [Pred / N]),
	    fail                               % backtrack to read_predicate1
	;   true
	), !,
	Cl \== end_of_file,                    % if end_of_file is read, fail
                                            % and backtrack to read_predicate
	define_predicate(Pred, N),
	group_clauses_by_pred(Pred, N, SrcCl, LSrcCl).

read_predicate1(Pred, N, [SrcCl|LSrcCl]) :-
	retract(buff_discontig_clause(Pred, N, SrcCl)),  % discontiguous pred
	                                                !,
	recover_discontig_clauses(Pred, N, LSrcCl).

read_predicate1(Pred, N, [SrcCl]) :-
	g_assign(reading_dyn_pred, t),
	retract(buff_dyn_interf_clause(Pred, N, SrcCl)),      % dyn predicate
	                                                 !.

read_predicate1(Pred, N, [SrcCl]) :-
	g_assign(reading_dyn_pred, t),
	retract(empty_dyn_pred(Pred, N, Where)),        % empty dyn predicate
	define_predicate(Pred, N),
	(   g_read(native_code, t) ->
	    create_dyn_interf_clause(Pred, N, Where, SrcCl)
	;   SrcCl = Where + '$$empty$$predicate$$clause$$'
	), !.

read_predicate1(Pred, N, LSrcCl) :-
	g_assign(reading_dyn_pred, f),
	retract(buff_exe_system(Where + Body)),       % system exe directives
	Pred = '$exe_system',
	N = 0,
	LSrcCl = [Where + (Pred :- Body)], !.

read_predicate1(Pred, N, LSrcCl) :-
	retract(buff_exe_user(Where + Body)),           % user exe directives
	Pred = '$exe_user',
	N = 0,
	LSrcCl = [Where + (Pred :- Body)], !.

read_predicate1(Pred, N, LSrcCl) :-
	Pred = end_of_file,
	N = 0,
	LSrcCl = [],                                            % end of file
	             !.




group_clauses_by_pred(Pred, N, SrcCl, [SrcCl|LSrcCl1]) :-
	get_next_clause(Pred1, N1, SrcCl1),
	(   Pred = Pred1,
	    N = N1 ->
	    group_clauses_by_pred(Pred1, N1, SrcCl1, LSrcCl1)
	;   LSrcCl1 = [],
	    (   Pred1 = end_of_file,
	        N1 = 0 ->
	        true
	    ;   asserta(buff_clause(Pred1, N1, SrcCl1))
	    )
	).




add_dyn_interf_clause(Pred, N, _) :-
	clause(buff_dyn_interf_clause(Pred, N, _), true),  % already asserted
	                                                  !.

add_dyn_interf_clause(Pred, N, Where) :-
	create_dyn_interf_clause(Pred, N, Where, SrcCl),
	assertz(buff_dyn_interf_clause(Pred, N, SrcCl)).




create_dyn_interf_clause(Pred, N, Where, SrcCl) :-
	length(LArgs, N),
	Head =.. [Pred|LArgs],
	SrcCl = Where + (Head :- call(Head)).




recover_discontig_clauses(Pred, N, [SrcCl|LSrcCl]) :-
	retract(buff_discontig_clause(Pred, N, SrcCl)), !,
	recover_discontig_clauses(Pred, N, LSrcCl).

recover_discontig_clauses(_, _, []).




create_exe_clauses_for_dyn_pred([], _, _).

create_exe_clauses_for_dyn_pred([SrcCl|LSrcCl], Pred, N) :-
	SrcCl = Where + Cl,
	add_wrapper_to_dyn_clause(Pred, N, Where + Cl, AuxName),
	handle_initialization(system, '$add_clause_term_and_bc'(Cl, [execute(AuxName / N)]), Where),
	create_exe_clauses_for_dyn_pred(LSrcCl, Pred, N).




create_exe_clauses_for_pub_pred([]).

create_exe_clauses_for_pub_pred([Where + Cl|LSrcCl]) :-
	handle_initialization(system, '$add_clause_term'(Cl), Where),
	create_exe_clauses_for_pub_pred(LSrcCl).




get_next_clause(Pred, N, SrcCl) :-
	retract(buff_clause(Pred, N, SrcCl)),
	SrcCl = Where + _,
	g_assign(where, Where), !.

get_next_clause(Pred, N, SrcCl) :-
	g_read(open_file_stack, OpenFileStack),
	OpenFileStack = [_ * Stream|_],
	'$catch'(read_term(Stream, Cl, [singletons(SingNames)]), error(syntax_error(Err), _), after_syn_error, any, 0, false),
	(   var(Err) ->
	    last_read_start_line_column(L1, _),
	    '$catch'(expand_term(Cl, Cl1), error(Err, _), dcg_error(Err), any, 0, false),
	    stream_line_column(Stream, Line, Col),
	    (   Col = 1 ->
	        L2 is Line - 1
	    ;   L2 = Line
	    ),
	    Where = OpenFileStack + (L1 - L2),
	    g_assign(where, Where),
	    get_next_clause1(Cl1, Where, SingNames, Pred, N, SrcCl)
	;   get_next_clause(Pred, N, SrcCl)
	), !.

get_next_clause1(end_of_file, _, _, Pred, N, SrcCl) :-
	close_last_prolog_file,
	g_read(open_file_stack, OpenFileStack),
	(   OpenFileStack = [] ->
	    Pred = end_of_file,
	    N = 0,
	    SrcCl = _ + end_of_file,
	    g_assign(eof_reached, t)
	;   get_next_clause(Pred, N, SrcCl)
	).

get_next_clause1((:- D), Where, SingNames, Pred, N, SrcCl) :-
	display_singletons(SingNames, directive),
	(   g_read(foreign_only, f)
	;   functor(D, foreign, _)
	),
	(   handle_directive(D, Where)
	;   error('invalid directive ~q', [D])
	), !,
	get_next_clause(Pred, N, SrcCl).

get_next_clause1(Cl, Where, SingNames, Pred, N, Where + Cl) :-
	g_read(foreign_only, f), !,
	(   Cl = (Head :- _)
	;   Cl = Head
	),
	(   nonvar(Head) ->
	    true
	;   error('head is a variable', [])
	),
	(   callable(Head) ->
	    true
	;   error('head is not a callable (~q)', [Head])
	),
	functor(Head, Pred, N),
	check_predicate(Pred, N),
	display_singletons(SingNames, Pred / N).

get_next_clause1(_, _, _, Pred, N, SrcCl) :-
                                          % ignore clause with --foreign-only
	get_next_clause(Pred, N, SrcCl).




after_syn_error :-
	g_read(syn_error_nb, SynErrNb),
	SynErrNb1 is SynErrNb + 1,
	g_assign(syn_error_nb, SynErrNb1),
	syntax_error_info(_, Line, Char, Msg),
	g_read(open_file_stack, OpenFileStack),
	g_assign(where, OpenFileStack + (Line - Line)),
	disp_msg('syntax error: ~a (char:~d)', [Msg, Char], error).




dcg_error(Err) :-
	last_read_start_line_column(Line, _),
	g_read(open_file_stack, OpenFileStack),
	g_assign(where, OpenFileStack + (Line - Line)),
	error('DCG error raised: ~w', [Err]).




display_singletons(SingNames, PI) :-
	g_read(singl_warn, t), !,
	get_singletons(SingNames, Sing),
	(   Sing = [] ->
	    true
	;   warn('singleton variables ~w for ~q', [Sing, PI])
	).

display_singletons(_, _).




get_singletons([], []).

get_singletons([X = _|SingNames], Sing1) :-
	(   sub_atom(X, 0, 1, _, '_') ->
	    Sing1 = Sing
	;   Sing1 = [X|Sing]
	),
	get_singletons(SingNames, Sing).




:-	discontiguous(handle_directive / 3).

handle_directive(D, Where) :-
	D =.. [DName|DLst],
	handle_directive(DName, DLst, Where).


handle_directive(public, DLst, _) :-
	!,
	DLst \== [],
	set_flag_for_preds(DLst, pub).

handle_directive(dynamic, DLst, Where) :-
	!,
	DLst \== [],
	set_flag_for_preds(DLst, dyn),
	set_flag_for_preds(DLst, pub),
	add_empty_dyn(DLst, Where).

handle_directive(discontiguous, DLst, _) :-
	!,
	DLst \== [],
	set_flag_for_preds(DLst, discontig).

handle_directive(built_in, DLst, _) :-
	!,
	(   DLst == [],
	    g_assign(default_kind, built_in)
	;   DLst \== [],
	    set_flag_for_preds(DLst, bpl)
	), !.

handle_directive(built_in_fd, DLst, _) :-
	!,
	(   DLst == [],
	    g_assign(default_kind, built_in_fd)
	;   DLst \== [],
	    set_flag_for_preds(DLst, bfd)
	).

handle_directive(ensure_linked, DLst, _) :-
	!,
	(   g_read(native_code, f) ->
	    warn('ensure_linked directive ignored in byte-code compilation mode', [])
	;   DLst \== [],
	    add_ensure_linked(DLst)
	).

handle_directive(multifile, _, _) :-
	!,
	warn('multifile directive not supported - directive ignored', []).

handle_directive(ensure_loaded, _, _) :-
	!,
	warn('ensure_loaded directive not supported - directive ignored', []).

handle_directive(include, [PlFile], _) :-
	!,
	open_new_prolog_file(PlFile).

handle_directive(op, [X, Y, Z], Where) :-
	!,
	exec_directive(op(X, Y, Z)),
	handle_initialization(system, op(X, Y, Z), Where).

handle_directive(char_conversion, [X, Y], Where) :-
	!,
	exec_directive(char_conversion(X, Y)),
	handle_initialization(system, char_conversion(X, Y), Where).

handle_directive(set_prolog_flag, [X, Y], Where) :-
	!,
	exec_directive(set_prolog_flag(X, Y)),
	(   current_prolog_flag(singleton_warning, off) ->
	    g_assign(singl_warn, f)
	;   g_assign(singl_warn, t)
	),
	handle_initialization(system, set_prolog_flag(X, Y), Where).

handle_directive(initialization, [Body], Where) :-
	!,
	handle_initialization(user, Body, Where).


handle_directive(foreign, [Template], Where) :-
	!,
	handle_directive(foreign, [Template, []], Where).

handle_directive(foreign, _, _) :-
	g_read(call_c, f), !,
	warn('foreign directive ignored in byte-code compilation mode', []).

handle_directive(foreign, [Template, Options], Where) :-
	!,
	callable(Template),
	list(Options),
	functor(Template, Pred, N),
	g_assign(foreign_fct_name, Pred),
	g_assign(foreign_return, boolean),
	g_assign(foreign_bip_name, Pred),
	g_assign(foreign_bip_arity, N),
	g_assign(foreign_choice_size, -1),
	foreign_get_options(Options),
	foreign_check_types(0, N, Template, LArgType),
	g_read(foreign_fct_name, FctName),
	g_read(foreign_return, Return),
	g_read(foreign_bip_name, BipName),
	g_read(foreign_bip_arity, BipArity),
	g_read(foreign_choice_size, ChcSize),
	functor(Head, Pred, N),
	SrcCl = Where + (Head :- '$foreign_call_c'(Head, LArgType, FctName, Return, BipName, BipArity, ChcSize)),
	assertz(buff_discontig_clause(Pred, N, SrcCl)),
	add_ensure_linked('$force_foreign_link' / 0).
                     % to force the link of foreign.o and then foreign_supp.o




foreign_get_options([]).

foreign_get_options([X|Options]) :-
	foreign_get_options1(X),
	foreign_get_options(Options).


foreign_get_options1(fct_name(FctName)) :-
	atom(FctName),
	g_assign(foreign_fct_name, FctName).

foreign_get_options1(return(Return)) :-
	atom(Return),
	(   Return = none
	;   Return = boolean
	;   Return = jump
	),
	g_assign(foreign_return, Return).

foreign_get_options1(bip_name(X)) :-
	nonvar(X),
	X = none,
	g_assign(foreign_bip_name, ''),
	g_assign(foreign_bip_arity, -2).

foreign_get_options1(bip_name(BipName, BipArity)) :-
	atom(BipName),
	integer(BipArity),
	g_assign(foreign_bip_name, BipName),
	g_assign(foreign_bip_arity, BipArity).

foreign_get_options1(choice_size(ChcSize)) :-
	integer(ChcSize),
	g_assign(foreign_choice_size, ChcSize).




foreign_check_types(N, N, _, []) :-
	!.

foreign_check_types(I, N, Template, [(A, Mode)|LArgType]) :-
	I1 is I + 1,
	arg(I1, Template, Arg),
	nonvar(Arg),
	(   Arg = + A,
	    Mode = 0
	;   Arg = - A,
	    Mode = 1
	;   Arg = ? A,
	    Mode = 2
	;   Arg = term,
	    A = term,
	    Mode = 0
	),                                                     % term = +term
	nonvar(A),
	foreign_check_arg(A),
	foreign_check_types(I1, N, Template, LArgType).

foreign_check_arg(integer).
foreign_check_arg(positive).
foreign_check_arg(float).
foreign_check_arg(number).
foreign_check_arg(atom).
foreign_check_arg(boolean).
foreign_check_arg(char).
foreign_check_arg(in_char).
foreign_check_arg(code).
foreign_check_arg(in_code).
foreign_check_arg(byte).
foreign_check_arg(in_byte).
foreign_check_arg(string).
foreign_check_arg(chars).
foreign_check_arg(codes).
foreign_check_arg(term).




handle_directive(DName, LArgs, _) :-
	length(LArgs, N),
	warn('unknown directive ~q - maybe use initialization/1 - directive ignored', [DName / N]).




handle_initialization(system, Body, Where) :-
	assertz(buff_exe_system(Where + Body)).

handle_initialization(user, Body, Where) :-
	assertz(buff_exe_user(Where + Body)).




exec_directive(Goal) :-
	'$catch'(Goal, Err, exec_directive_exception(Goal, Err), any, 0, false), !.

exec_directive(Goal) :-
	warn('directive failed (~q)', [Goal]).

exec_directive_exception(Goal, Err) :-
	warn('directive failed (~q) with exception (~q)', [Goal, Err]).



used_bips_via_call :-                     % to enforce the link of these bips
	op(_, _, _),
	char_conversion(_, _),
	set_prolog_flag(_, _),
	expand_term(_, _).




add_empty_dyn([], _) :-
	!.

add_empty_dyn([P1|P2], Where) :-
	!,
	add_empty_dyn(P1, Where),
	add_empty_dyn(P2, Where).

add_empty_dyn((P1, P2), Where) :-
	!,
	add_empty_dyn(P1, Where),
	add_empty_dyn(P2, Where).

add_empty_dyn(Pred / N, Where) :-
	assertz(empty_dyn_pred(Pred, N, Where)).




add_ensure_linked([]) :-
	!.

add_ensure_linked([P1|P2]) :-
	!,
	add_ensure_linked(P1),
	add_ensure_linked(P2).

add_ensure_linked((P1, P2)) :-
	!,
	add_ensure_linked(P1),
	add_ensure_linked(P2).

add_ensure_linked(Pred / N) :-
	clause(ensure_linked(Pred, N), true), !.

add_ensure_linked(Pred / N) :-
	assertz(ensure_linked(Pred, N)).




set_flag_for_preds([], _) :-
	!.

set_flag_for_preds([P1|P2], Flag) :-
	!,
	set_flag_for_preds(P1, Flag),
	set_flag_for_preds(P2, Flag).

set_flag_for_preds((P1, P2), Flag) :-
	!,
	set_flag_for_preds(P1, Flag),
	set_flag_for_preds(P2, Flag).

set_flag_for_preds(Pred / N, Flag) :-
	atom(Pred),
	integer(N),
	(   test_pred_info(def, Pred, N) ->
	    warn('directive occurs after definition of ~q - directive ignored', [Pred / N])
	;   (   Flag = bpl,
	        unset_pred_info(bfd, Pred, N)
	    ;   Flag = bfd,
	        unset_pred_info(bpl, Pred, N)
	    ;   true
	    ), !,
	    set_pred_info(Flag, Pred, N)
	).




define_predicate(F, N) :-
	set_pred_info(def, F, N),
	test_pred_info(bpl, F, N), !.

define_predicate(F, N) :-
	test_pred_info(bfd, F, N), !.

define_predicate(F, N) :-
	g_read(default_kind, built_in), !,
	set_pred_info(bpl, F, N).

define_predicate(F, N) :-
	g_read(default_kind, built_in_fd), !,
	set_pred_info(bfd, F, N).

define_predicate(_, _).




flag_bit(def, 0).
flag_bit(dyn, 1).
flag_bit(pub, 2).
flag_bit(bpl, 3).
flag_bit(bfd, 4).
flag_bit(discontig, 5).
flag_bit(cut, 6).




set_pred_info(Flag, F, N) :-
	flag_bit(Flag, Bit),
	(   retract(pred_info(F, N, X))
	;   X = 0
	), !,
	X1 is X \/ 1 << Bit,
	assertz(pred_info(F, N, X1)).




unset_pred_info(Flag, F, N) :-
	flag_bit(Flag, Bit),
	retract(pred_info(F, N, X)), !,
	X1 is X /\ \ (1 << Bit),
	assertz(pred_info(F, N, X1)).

unset_pred_info(_, _, _).




test_pred_info(Flag, F, N) :-
	flag_bit(Flag, Bit),
	clause(pred_info(F, N, X), _),
	X /\ 1 << Bit > 0 .




check_predicate(Pred, N) :-
	g_read(redef_error, t),
	bip(Pred, N), !,
	error('redefining built-in predicate ~q', [Pred / N]).

check_predicate(Pred, N) :-
	g_read(redef_error, t),
	control_construct(Pred, N), !,
	error('redefining control construct ~q', [Pred / N]).

check_predicate(Pred, N) :-
	g_read(susp_warn, t),
	suspicious_predicate(Pred, N), !,
	warn('suspicious predicate ~q', [Pred / N]).

check_predicate(Pred, N) :-
	'$aux_name'(Pred), !,
	warn('using system auxiliary predicate ~q', [Pred / N]).

check_predicate(_, _).




control_construct(',', 2).
control_construct(;, 2).
control_construct(->, 2).
control_construct(!, 0).
control_construct(fail, 0).
control_construct(true, 0).
control_construct(call, 1).
control_construct(catch, 3).
control_construct(throw, 1).

suspicious_predicate(',', 2).
suspicious_predicate(;, 2).
suspicious_predicate(->, 2).
suspicious_predicate(!, 0).
suspicious_predicate(:-, 1).
suspicious_predicate(:-, 2).
suspicious_predicate(-->, 2).
suspicious_predicate({}, X) :-
	X < 2 .
suspicious_predicate(+, 2).
suspicious_predicate(-, 2).
suspicious_predicate(*, 2).
suspicious_predicate(/, 2).
suspicious_predicate(//, 2).




warn(Msg, LArg) :-
	disp_msg(Msg, LArg, warning).




error(Msg, LArg) :-
	disp_msg(Msg, LArg, 'fatal error'),
	repeat,                                      % close all opened files
	(   close_last_prolog_file ->
	    fail
	;   !
	),
	abort.




disp_msg(Msg, LArg, MsgType) :-
	numbervars(LArg),
	g_read(where, Where),
	(   Where = OpenFileStack + L12,
	    L12 = _ - _ ->
	    disp_file_name(OpenFileStack, _),
	    disp_lines(L12)
	;   true
	),
	format('~a: ', [MsgType]),
	format(Msg, LArg),
	nl(user_output).




disp_file_name([], '') :-
	!.

disp_file_name([FileName * _|OpenFileStack], ' including ') :-
	disp_file_name(OpenFileStack, Before),
	format('~a~a', [Before, FileName]).




disp_lines(L - L) :-
	!,
	format(':~d ', [L]).

disp_lines(L1 - L2) :-
	format(':~d--~d ', [L1, L2]).




          /*--- exception recovery ---*/

exception(error(syntax_error(_), _)) :-
	!,
	syntax_error_info(_, Line, Char, Msg),
	g_read(open_file_stack, OpenFileStack),
	g_assign(where, OpenFileStack + (Line - Line)),
	error('syntax error: ~a (char:~d)', [Msg, Char]).

exception(error(existence_error(source_sink, File), _)) :-
	!,
	error('cannot open file ~a - does not exist', [File]).

exception(error(permission_error(open, source_sink, File), _)) :-
	!,
	error('cannot open file ~a - permission error', [File]).

exception(Err) :-
	error('exception raised: ~q', [Err]).
