/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint definition file to C code compiler                *
 * File  : compile.pl                                                      *
 * Descr.: final compilation and emission                                  *
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


        % to correctly write C expressions

:-	op(750, yfx, [&&, '||']).
:-	op(700, xfx, [==, '!=', <=]).
:-	op(400, yfx, [/<, />, '%']).
:-	op(300, fy, [&]).


emit_code_init(FdFile) :-
	absolute_file_name(FdFile, FdFile1),
	format(stream_c, '/* C file generated for ~w */~n~n', [FdFile1]),
	format(stream_c, '#include "fd_to_c.h"~n', []).




emit_user_cstr(uc(Name, AFSize, LHVar, Body)) :-
	g_assign(name, Name),
	g_assign(bloc, 1),
	atom_length(Name, X),
	X1 is X + 19,
	format(stream_c, '~n\t/*~*c*/~n', [X1, 45]),
	format(stream_c, '\t/* User constraint: ~a */~n', [Name]),
	format(stream_c, '\t/*~*c*/~n', [X1, 45]),
	e_body(Body, LFctName),
	format(stream_c, '~n~n\t/* Entry point for ~a */~n', [Name]),
	format(stream_c, '~nfd_begin_user_constraint(~a', [Name]),
	e_head_vars(LHVar, '('),
	format(stream_c, '))~n~n', []),
	format(stream_c, '   fd_create_a_frame(~d)~n', [AFSize]),
	e_load_env(LHVar),
	nl(stream_c),
	format(stream_c, '   fd_before_add_constraint~n', []),
	e_call_fct_lst(LFctName),
	format(stream_c, '   fd_after_add_constraint~n', []),
	format(stream_c, ' fd_exit_point~n', []),
	format(stream_c, '   fd_return~n', []),
	format(stream_c, '~nfd_end_user_constraint~n~n', []).




e_head_vars([], _).

e_head_vars([v(V, _, _)|LHVar], Car) :-
	format(stream_c, '~aFdArg(~a)', [Car, V]),
	e_head_vars(LHVar, ',').




e_load_env([]).

e_load_env([v(V, T, I)|LHVar]) :-
	format(stream_c, '   fd_~a_in_a_frame(~a,~d)~n', [T, V, I]),
	e_load_env(LHVar).




e_call_fct_lst([]).

e_call_fct_lst([FctName|LFctName]) :-
	e_call_internal(FctName, f),
	e_call_fct_lst(LFctName).



e_call_internal(FctName, CallerHasCFrame) :-
	(   sub_atom(FctName, _, _, _, '_switch_') ->
	    (   CallerHasCFrame = t ->
	        Inst = fd_call_internal_and_test_switch
	    ;   Inst = fd_call_internal_and_test_switch_simple
	    )
	;   Inst = fd_call_internal
	),
	format(stream_c, '   ~a(~a)~n', [Inst, FctName]).




e_body(by(LBloc, WaitSwt), LFctName1) :-
	e_bloc_lst(LBloc, LFctName),
	(   WaitSwt = ws(LUse, LCase) ->
	    e_wait_swt(LUse, LCase, FctName1),
	    e_fct_install_triggers(-1, LUse, -1, always, FctName1, FctName),
	    append(LFctName, [FctName], LFctName1)
	;   LFctName1 = LFctName
	).




e_wait_swt(LUse, LCase, FctName) :-
	e_case_lst(LCase, 1, LFctName0),
	internal_fct_name(switch, 1, FctName),
	e_bloc_load_use(LUse, LCVarUsed, LWInst1, LWInst),
	e_test_case_lst(LCase, LFctName0, LWInst1),
	alloc_and_emit_fct_code(FctName, LCVarUsed, LWInst, t).




e_case_lst([], _, []).

e_case_lst([ca(_, LBNoStop, LBloc)|LCase], I, [FctName|LFctName]) :-
	e_bloc_lst(LBloc, LFctName0),
	(   LFctName0 = [FctName],
	    LBNoStop = [] ->
	    true
	;   e_case_group(LFctName0, LBNoStop, I, FctName)
	),
	I1 is I + 1,
	e_case_lst(LCase, I1, LFctName).




e_case_group(LFctName, LBNoStop, I, FctName) :-
	internal_fct_name(case_group, I, FctName),
	format(stream_c, '~n~n\t/* Group of case #~d */~n', [I]),
	format(stream_c, '~nfd_begin_internal(~a)~n~n', [FctName]),
	e_stop_lst(LBNoStop),
	e_call_fct_lst(LFctName),
	format(stream_c, ' fd_exit_point~n', []),
	format(stream_c, '   fd_return~n', []),
	format(stream_c, '~nfd_end_internal~n', []).




e_stop_lst([]).

e_stop_lst([BNoStop|LBNoStop]) :-
	format(stream_c, '   fd_stop_constraint(~d)~n', [BNoStop]),
	e_stop_lst(LBNoStop).




e_test_case_lst([], [], []).

e_test_case_lst([ca(co(Term, LWNext, LWInst), _, _)|LCase], [FctName|LFctName], LWInst) :-
	LWNext = [fd_test_switch_condition(Term, FctName)|LWInst1],
	e_test_case_lst(LCase, LFctName, LWInst1).




e_bloc_lst([], []).

e_bloc_lst([Bloc|LBloc], [FctName|LFctName]) :-
	g_read(bloc, BlNo),
	format(stream_c, '~n~n\t/* Bloc #~d */~n', [BlNo]),
	e_bloc_one(Bloc, FctName),
	BlNo1 is BlNo + 1,
	g_assign(bloc, BlNo1),
	e_bloc_lst(LBloc, LFctName).




e_bloc_one(bl(BNo, LDep, LUse, LWInst, TellFdv, Always), FctName1) :-
	e_bloc(LUse, LWInst, FctName),
	e_fct_install_triggers(BNo, LDep, TellFdv, Always, FctName, FctName1).




e_bloc(LUse, LWInst1, FctName) :-
	g_read(bloc, BlNo),
	BlNo1 is BlNo + 1,
	g_assign(bloc, BlNo1),
	internal_fct_name(bloc, BlNo, FctName),
	e_bloc_load_use(LUse, LCVarUsed, LWInst1, LWInst),
	close_list(LCVarUsed),
	simplif_code(LWInst, LWSimpl),
	alloc_and_emit_fct_code(FctName, LCVarUsed, LWSimpl, t).




e_fct_install_triggers(BNo, LDep, TellFdv, Always, FctName, FctName1) :-
	e_has_dependencies(LDep), !,
	(   Always = always ->
	    Optim = 0
	;   Optim = 1
	),
	atom_concat(FctName, '_inst', FctName1),
	format(stream_c, '~nfd_begin_internal(~a)~n~n', [FctName1]),
	format(stream_c, '   fd_local_cf_pointer~n', []),
	format(stream_c, '   fd_create_c_frame(~a,~d,~d)~n', [FctName, TellFdv, Optim]),
	(   BNo = -1 ->
	    true
	;   format(stream_c, '   fd_cf_in_a_frame(~d)~n', [BNo])
	),
	e_install_trig(LDep),
	e_call_internal(FctName, t),
	format(stream_c, ' fd_exit_point~n', []),
	format(stream_c, '   fd_return~n', []),
	format(stream_c, '~nfd_end_internal~n', []).

e_fct_install_triggers(_, _, _, _, FctName, FctName).




e_install_trig([]).

e_install_trig([m(_, fdv, I, i(Min, Max, Dom, Val))|LDep]) :-
	e_compute_dep_chain(Min, Max, Dom, Val, Chain), !,
	format(stream_c, '   fd_add_dependency(~d,~a)~n', [I, Chain]),
	e_install_trig(LDep).

e_install_trig([m(_, l_fdv, I, i(Min, Max, Dom, Val))|LDep]) :-
	e_compute_dep_chain(Min, Max, Dom, Val, Chain), !,
	format(stream_c, '   fd_add_list_dependency(~d,~a)~n', [I, Chain]),
	e_install_trig(LDep).

e_install_trig([_|LDep]) :-
	e_install_trig(LDep).





e_has_dependencies([m(_, T, _, i(Min, Max, Dom, Val))|_]) :-
	(   T = fdv
	;   T = l_fdv
	),
	e_compute_dep_chain(Min, Max, Dom, Val, _), !.

e_has_dependencies([_|LDep]) :-
	e_has_dependencies(LDep).




e_bloc_load_use([], _, LWNext, LWNext) :-
	!.

e_bloc_load_use([m(V, T, I, Mark)|LUse], LCVarUsed, LWNext, LWInst) :-
	e_bloc_load_one(T, V, I, Mark, LCVarUsed, LWInst1, LWInst),
	e_bloc_load_use(LUse, LCVarUsed, LWNext, LWInst1).




e_bloc_load_one(fdv, V, I, Mark, LCVarUsed, LWNext, LWInst) :-
	!,
	Mark = i(Min, Max, Dom, Val),
	e_compute_dep_chain(Min, Max, Dom, Val, Chain),
	e_bloc_load_fdv(Chain, Min, Max, Dom, Val, V, I, LCVarUsed, LWNext, LWInst).

e_bloc_load_one(range, _, I, range(R), _, LWNext, LWInst) :-
	!,
	(   nonvar(I) ->
	    LWInst = [fd_load_range(R, I)|LWNext]
	;   LWInst = LWNext
	).

e_bloc_load_one(T, V, I, _, LCVarUsed, LWNext, [WInst|LWNext]) :-
	nonvar(I), !,
	atom_concat(fd_load_, T, F),
	use_c_var(T, V, LCVarUsed, A),
	WInst =.. [F, A, I].

e_bloc_load_one(T, V, _, _, LCVarUsed, LWNext, LWNext) :-
	use_c_var(T, V, LCVarUsed, _).




use_c_var(T, V, LCVarUsed, A) :-
	atom_concat(T, V, A),
	memberchk(cv(T, A), LCVarUsed).




e_bloc_load_fdv(val, Min, Max, Dom, _, V, I, LCVarUsed, LWNext, LWInst) :-
	use_c_var(val, V, LCVarUsed, AVal),
	LWInst = [fd_load_val(AVal, I)|LWInst1],
	(   nonvar(Dom) ->
	    Dom = t(R),
	    LWInst1 = [fd_range_interval(R, AVal, AVal)|LWInst2]
	;   LWInst1 = LWInst2
	),
	(   nonvar(Min) ->
	    use_c_var(min, V, LCVarUsed, AMin),
	    LWInst2 = [fd_value_copy(AMin, AVal)|LWInst3]
	;   LWInst2 = LWInst3
	),
	(   nonvar(Max) ->
	    use_c_var(max, V, LCVarUsed, AMax),
	    LWInst3 = [fd_value_copy(AMax, AVal)|LWNext]
	;   LWInst3 = LWNext
	).

e_bloc_load_fdv(dom, Min, Max, t(R), _, V, I, LCVarUsed, LWNext, LWInst) :-
	LWInst = [fd_load_dom(R, I)|LWInst1],
	(   nonvar(Min) ->
	    use_c_var(min, V, LCVarUsed, AMin),
	    LWInst1 = [fd_min_of_range(AMin, R)|LWInst2]
	;   LWInst1 = LWInst2
	),
	(   nonvar(Max) ->
	    use_c_var(max, V, LCVarUsed, AMax),
	    LWInst2 = [fd_max_of_range(AMax, R)|LWNext]
	;   LWInst2 = LWNext
	).

e_bloc_load_fdv(min_max, _, _, _, _, V, I, LCVarUsed, LWNext, LWInst) :-
	use_c_var(min, V, LCVarUsed, AMin),
	use_c_var(max, V, LCVarUsed, AMax),
	LWInst = [fd_load_min_max(AMin, AMax, I)|LWNext].

e_bloc_load_fdv(min, _, _, _, _, V, I, LCVarUsed, LWNext, LWInst) :-
	use_c_var(min, V, LCVarUsed, AMin),
	LWInst = [fd_load_min(AMin, I)|LWNext].

e_bloc_load_fdv(max, _, _, _, _, V, I, LCVarUsed, LWNext, LWInst) :-
	use_c_var(max, V, LCVarUsed, AMax),
	LWInst = [fd_load_max(AMax, I)|LWNext].




e_compute_dep_chain(_, _, _, Val, val) :-
	nonvar(Val),                                               % val used
	             !.

e_compute_dep_chain(_, _, Dom, _, dom) :-
	nonvar(Dom),                                               % dom used
	             !.

e_compute_dep_chain(Min, Max, _, _, min) :-
	nonvar(Min),
	var(Max),                                             % only min used
	          !.

e_compute_dep_chain(Min, Max, _, _, max) :-
	var(Min),
	nonvar(Max),                                          % only max used
	             !.

e_compute_dep_chain(Min, Max, _, _, min_max) :-            % min and max used
	nonvar(Min),
	nonvar(Max).




simplif_code([], []).

simplif_code([fd_range_compl(R1), fd_range_compl(R2)|LWInst], LWSimpl) :-
	R1 == R2, !,
	simplif_code(LWInst, LWSimpl).

simplif_code([fd_range_interval(R1, T1, T2), fd_tell_range(I, R2)|LWInst], LWSimpl) :-
	R1 == R2, !,
	simplif_code([fd_tell_interval(I, T1, T2)|LWInst], LWSimpl).

simplif_code([fd_tell_interval(I, T1, T2)|LWInst], LWSimpl) :-
	T1 == T2, !,
	simplif_code([fd_tell_value(I, T1)|LWInst], LWSimpl).

simplif_code([fd_range_full(R1), fd_range_reset_value(R2, T), fd_tell_range(I, R3)|LWInst], LWSimpl) :-
	R1 == R2,
	R1 == R3, !,
	simplif_code([fd_tell_not_value(I, T)|LWInst], LWSimpl).

simplif_code([fd_range_empty(R1)|LWInst], LWSimpl1) :-
	simpl_get_lst(LWInst, R1, LWReset, LWInst1),
	simplif_code(LWInst1, LWInst2),
	(   LWInst2 = [fd_range_compl(R2)|LWInst3],           % compl of list
	    R1 == R2,
	    append([fd_range_full(R1)|LWReset], LWInst3, LWSimpl)
	;   LWReset = [WReset],                            % only one element
	    arg(2, WReset, Term),
	    LWSimpl = [fd_range_interval(R1, Term, Term)|LWInst2]
	), !,
	simplif_code(LWSimpl, LWSimpl1).

simplif_code([fd_range_interval(R1, T1, T2), fd_range_union(R2, R3)|LWInst], LWSimpl1) :-
	T1 == T2,                          % R union {value} = set value in R
	R1 == R3,
	LWSimpl = [fd_range_set_value(R2, T1)|LWInst],
	simplif_code(LWSimpl, LWSimpl1).

simplif_code([WInst|LWInst], [WInst|LWInst1]) :-
	simplif_code(LWInst, LWInst1).




simpl_get_lst([fd_range_set_value(R2, Term)|LWInst], R1, [WReset|LWReset], LWInst1) :-
	R1 == R2, !,
	functor(WReset, fd_range_reset_value, 2),
	arg(1, WReset, R2),
	arg(2, WReset, Term),
	simpl_get_lst(LWInst, R1, LWReset, LWInst1).

simpl_get_lst(LWInst, _, [], LWInst).




internal_fct_name(Kind, No, FctName) :-
	g_read(name, Name),
	number_atom(No, ANo),
	atom_concat(Name, '_', X1),
	atom_concat(X1, Kind, X2),
	atom_concat(X2, '_', X3),
	atom_concat(X3, ANo, FctName).




alloc_and_emit_fct_code(FctName, LCVarUsed, LWInst, LocalFdvAdr) :-
	format(stream_c, '~nfd_begin_internal(~a)~n~n', [FctName]),
	allocate_registers(LWInst, MaxUsedReg),
	(   LocalFdvAdr = t ->
	    format(stream_c, '   fd_local_fdv_adr~n', [])
	;   true
	),
	emit_c_vars_lst(LCVarUsed),
	emit_used_regs_lst(MaxUsedReg),
	nl(stream_c),
	(   MaxUsedReg >= 0 ->
	    format(stream_c, '   fd_allocate~n', []),
	    Alloc = t
	;   Alloc = f
	),
	emit_inst_lst(LWInst, Alloc),
	format(stream_c, '~nfd_end_internal~n', []).




emit_c_vars_lst([]).

emit_c_vars_lst([cv(T, A)|LCVarUsed]) :-
	(   (   T = int
	    ;   T = val
	    ;   T = min
	    ;   T = max
	    ) ->
	    I1 = value
	;   I1 = T
	),
	format(stream_c, '   fd_local_~a_var(~a)~n', [I1, A]),
	emit_c_vars_lst(LCVarUsed).



emit_used_regs_lst(MaxUsedReg) :-
	for(I, 0, MaxUsedReg),
	format(stream_c, '   fd_local_range_var(~d)~n', [I]),
	fail.

emit_used_regs_lst(_).




emit_inst_lst([], Alloc) :-
	format(stream_c, ' fd_exit_point~n', []),
	(   Alloc = t ->
	    format(stream_c, '   fd_deallocate~n', [])
	;   true
	),
	format(stream_c, '   fd_return~n', []).

emit_inst_lst([WInst|LWInst], Alloc) :-
	dummy_instruction(WInst), !,
	emit_inst_lst(LWInst, Alloc).

emit_inst_lst([WInst|LWInst], t) :-
	functor(WInst, F, _),
	(   sub_atom(F, 0, _, _, fd_tell_)
	;   sub_atom(F, 0, _, _, fd_check_fct)
	), !,
	format(stream_c, '   fd_deallocate~n', []),
	format(stream_c, '   ~w~n', [WInst]),
	emit_inst_lst(LWInst, already).

emit_inst_lst([WInst|LWInst], Alloc) :-
	Alloc \== f,
	functor(WInst, F, _),
	sub_atom(F, 0, _, _, fd_forall_end), !,
	format(stream_c, '   fd_allocate~n', []),
	format(stream_c, '   ~w~n', [WInst]),
	emit_inst_lst(LWInst, f).

emit_inst_lst([WInst|LWInst], Alloc) :-
	format(stream_c, '   ~w~n', [WInst]),
	emit_inst_lst(LWInst, Alloc).




:-	include('../Pl2Wam/reg_alloc.pl').

          % alias stopping instructions

alias_stop_instruction(_) :-
	fail.



          % instruction codification

codification(WamInst, LCode) :-
	codif(WamInst, LCode), !.




          % FD instructions using Ranges


codif(fd_tell_range(_, RR), [r(RR)]).


codif(fd_load_range(RR, _), [w(RR)]).

codif(fd_load_dom(RR, _), [w(RR)]).
codif(fd_min_of_range(_, RR), [r(RR)]).
codif(fd_max_of_range(_, RR), [r(RR)]).

codif(fd_range_interval(RR, _, _), [w(RR)]).

codif(fd_range_union(RR, RR1), [r(RR), r(RR1), w(RR)]).
codif(fd_range_inter(RR, RR1), [r(RR), r(RR1), w(RR)]).

codif(fd_range_compl(RR), [r(RR), w(RR)]).

codif(fd_range_empty(RR), [w(RR)]).
codif(fd_range_set_value(RR, _), [r(RR), w(RR)]).
codif(fd_range_full(RR), [w(RR)]).
codif(fd_range_reset_value(RR, _), [r(RR), w(RR)]).

codif(fd_range_add_range(RR, RR1), [r(RR), r(RR1), w(RR)]).
codif(fd_range_sub_range(RR, RR1), [r(RR), r(RR1), w(RR)]).
codif(fd_range_mul_range(RR, RR1), [r(RR), r(RR1), w(RR)]).
codif(fd_range_div_range(RR, RR1), [r(RR), r(RR1), w(RR)]).
codif(fd_range_mod_range(RR, RR1), [r(RR), r(RR1), w(RR)]).

codif(fd_range_add_value(RR, _), [r(RR), w(RR)]).
codif(fd_range_sub_value(RR, _), [r(RR), w(RR)]).
codif(fd_range_mul_value(RR, _), [r(RR), w(RR)]).
codif(fd_range_div_value(RR, _), [r(RR), w(RR)]).
codif(fd_range_mod_value(RR, _), [r(RR), w(RR)]).

codif(fd_range_copy(RR, RR1), [c(RR1, RR)]).

codif(fd_range_fct(_, RR, FdArg), [w(RR)|LCode]) :-
	FdArg =.. [_|FdLstArg],
	create_r_code_lst(FdLstArg, [], LCode).

codif(X, LCode) :-
	create_r_code_lst(X, [], LCode).






create_r_code_lst(range_arg(RR), LNext, [r(RR)|LNext]) :-
	!.

create_r_code_lst(T, LNext, LNext) :-
	atomic(T), !.

create_r_code_lst(T, LNext, LCode) :-
	compound(T), !,
	functor(T, _, A),
	create_r_code_lst1(0, A, T, LNext, LCode).


create_r_code_lst1(I, A, T, LNext, LCode) :-
	(   I = A ->
	    LCode = LNext
	;   I1 is I + 1,
	    arg(I1, T, T1),
	    create_r_code_lst(T1, LCode1, LCode),
	    create_r_code_lst1(I1, A, T, LNext, LCode1)
	).





          % dummy instructions


dummy_instruction(fd_range_copy(R, R)).
