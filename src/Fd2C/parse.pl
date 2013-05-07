/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : FD constraint definition file to C code compiler                *
 * File  : parse.pl                                                        *
 * Descr.: parsing and some code emission                                  *
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


parse_user_cstr(uc(Name, AFSize, LHVar, Body)) -->
	head(Name, NbHVar, LHVar),
	terminal_check('{'),
	{ g_assign(afsize, NbHVar), retractall(hvar(_)), asserta(hvar(LHVar)), retractall(bname(_, _)) },
	body(Body),
	terminal_check('}'), !,
	{ g_read(afsize, AFSize), close_list(LHVar) }.




head(Name, NbHVar, LHVar) -->
	ident_check(Name),
	terminal_check('('),
	decl_lst(0, NbHVar, LHVar),
	terminal_check(')'), !.




decl_lst(I, NbHVar, [HVar|LHVar]) -->
	decl_one(I, HVar),
	{ I1 is I + 1 },
	decl_rest_lst(I1, NbHVar, LHVar).


decl_rest_lst(I, NbHVar, LHVar) -->
	terminal(','), !,
	decl_lst(I, NbHVar, LHVar).

decl_rest_lst(I, I, _) -->
	[].


decl_one(I, v(V, T, I)) -->
	type(T),
	a_var(V), !.

decl_one(_, _) -->
	syn_error('variable declaration').




type(int) -->
	terminal(int).
type(range) -->
	terminal(range).
type(fdv) -->
	terminal(fdv).
type(any) -->
	terminal(any).
type(l_int) -->
	terminal(l_int).
type(l_range) -->
	terminal(l_range).
type(l_fdv) -->
	terminal(l_fdv).
type(l_any) -->
	terminal(l_any).




body(by(LBloc, WaitSwt)) -->
	bloc_lst(LBloc),
	wait_swt(WaitSwt), !.

body(_) -->
	syn_error(body).



wait_swt(ws(LUse, LCase)) -->
	terminal(wait_switch), !,
	{ clause(hvar(LVar), _) },
	case_lst(LVar, LUse, LCase),
	{ close_list(LUse) }.

wait_swt(no_wait_switch) -->
	[].



case_lst(LVar, LUse, [Case|LCase]) -->
	case_one(LVar, LUse, Case),
	case_rest_lst(LVar, LUse, LCase).



case_rest_lst(LVar, LUse, LCase) -->
	case_lst(LVar, LUse, LCase), !.

case_rest_lst(_, _, []) -->
	[].


case_one(LVar, LUse, ca(co(Term, LWNext, LWInst), LBNoStop, LBloc)) -->
	terminal(case),
	term(LVar, LUse, Term, LWNext, LWInst),
	stop_lst(LBNoStop),
	bloc_lst(LBloc).




stop_lst([BNoStop|LBNoStop]) -->
	stop_one(BNoStop),
	stop_lst(LBNoStop).

stop_lst([]) -->
	[].


stop_one(BNoStop) -->
	terminal(stop), !,
	ident_check(BName),
	(   { clause(bname(BName, BNoStop), _) } ->
	    { true }
	;   sem_error('undeclared bloc name "~a"', [BName])
	).




bloc_lst([Bloc|LBloc]) -->
	bloc_one(Bloc),
	bloc_lst(LBloc).

bloc_lst([]) -->
	[].




bloc_one(bl(BNo, LDep, LUse, LWInst, TellFdv, Always)) -->
	{ clause(hvar(LVar), _) },
	terminal(start),
	bloc_name(BNo),
	elem_lst(LVar, LUse, LWInst1, LWInst),
	forall(LVar, LUse, LWInst2, LWInst1, HasForAll),
	last_elem(LVar, LUse, TellFdv, LWInst3, LWInst2),
	{ HasForAll = t -> LWInst3 = [fd_forall_end] ; LWInst3 = [] },
	trig(LVar, LUse, LDep),
	always(Always),
	{ close_list(LDep), close_list(LUse) }.




bloc_name(BNo) -->
	terminal('('), !,
	ident_check(BName),
	(   { clause(bname(BName, _), _) } ->
	    sem_error('bloc name already used "~a"', [BName])
	;   { g_read(afsize, BNo), AFSize is BNo + 1, g_assign(afsize, AFSize), assertz(bname(BName, BNo)) }
	),
	terminal_check(')').

bloc_name(-1) -->
	[].




trig(LVar, LUse, LDep) -->
	terminal(trigger),
	terminal(also), !,
	terminal_check(on),
	trig_lst(LVar, LDep),
	{ add_use_to_dep(LUse, LDep) }.

trig(LVar, _, LDep) -->
	terminal(trigger), !,
	terminal_check(on),
	trig_lst(LVar, LDep).

trig(_, LUse, LDep) -->
	{ add_use_to_dep(LUse, LDep) }.




trig_lst(LVar, LDep) -->
	trig_one(LVar, LDep),
	trig_rest_lst(LVar, LDep).


trig_rest_lst(LVar, LDep) -->
	terminal(','),
	trig_lst(LVar, LDep), !.

trig_rest_lst(_, _) -->
	[].


trig_one(LVar, LDep) -->
	terminal(What),
	{ What = min ; What = max ; What = dom ; What = val }, !,
	terminal_check('('),
	a_var(V),
	terminal_check(')'),
	{ get_typeof(LVar, V, T, I), (T \== fdv, T \== l_fdv -> var_check_type(LVar, V, fdv, _, _, _)
                                                                 % type error
	                                                                                              ; true), (What = dom -> What1 = dom(_) ; What1 = What), add_marked_var(LDep, V, T, I, What1) }.




always(always) -->
	terminal(always), !.

always(optimized) -->
	[].




elem_lst(LVar, LUse, LWNext, LWInst) -->
	elem_one(LVar, LUse, LWInst1, LWInst),
	elem_lst(LVar, LUse, LWNext, LWInst1).

elem_lst(_, _, LWNext, LWNext) -->
	[].




elem_one(LVar, LUse, LWNext, LWInst) -->
	type(T),
	a_var(V), !,
	check_type_int_or_range(T),
	{ add_local_var(LVar, v(V, T, I)) },
	local_var_assign(LVar, LUse, V, T, I, LWNext, LWInst).

elem_one(LVar, LUse, LWNext, LWInst) -->
	a_var(V),
	terminal(=), !,
	{ get_typeof(LVar, V, T, I) },
	check_type_int_or_range(T),
	assign_right_value(LVar, LUse, V, T, I, LWNext, LWInst).

elem_one(LVar, LUse, LWNext, LWInst) -->
	terminal(fail), !,
	terminal_check(if),
	term(LVar, LUse, Term, LWInst1, LWInst),
	{ LWInst1 = [fd_test_fail_condition(Term)|LWNext] }.

elem_one(LVar, LUse, LWNext, LWInst) -->
	terminal(exit), !,
	terminal_check(if),
	term(LVar, LUse, Term, LWInst1, LWInst),
	{ LWInst1 = [fd_test_exit_condition(Term)|LWNext] }.




check_type_int_or_range(int) -->
	!.

check_type_int_or_range(range) -->
	!.

check_type_int_or_range(_) -->
	syn_error('int or range type').




local_var_assign(LVar, LUse, V, T, I, LWNext, LWInst) -->
	terminal(=), !,
	assign_right_value(LVar, LUse, V, T, I, LWNext, LWInst).

local_var_assign(_, _, _, _, _, LWNext, LWNext) -->
	[].




assign_right_value(LVar, LUse, V, int, I, LWNext, LWInst) -->
	{ atom_concat(int, V, X), add_marked_var(LUse, V, int, I, _) },
	term(LVar, LUse, Term, LWInst, LWInst1), !,
	{ LWInst1 = [fd_init_local_value_var(X, Term)|LWNext] }.

assign_right_value(LVar, LUse, V, range, I, LWNext, LWInst) -->
	{ add_marked_var(LUse, V, range, I, range(Range)) },
	range(LVar, LUse, Range, LWNext, LWInst), !.

assign_right_value(_, _, _, _, _, _, _) -->
	syn_error('right value').




forall(LVar, LUse, LWNext, LWInst, t) -->
	terminal(forall), !,
	var_check_new(LVar, V),
	terminal_check(of),
	var_check_type(LVar, VL, l_fdv, IL),
	terminal_check(do),
	{ add_marked_var(LUse, VL, l_fdv, IL, _), g_read(afsize, I), AFSize is I + 1, g_assign(afsize, AFSize), add_local_var(LVar, v(V, fdv, I)), atom_concat(l_fdv, VL, VL1), LWInst = [fd_forall(I, VL1)|LWNext] }.

forall(_, _, LWNext, LWNext, f) -->
	[].




last_elem(LVar, LUse, -1, LWNext, LWInst) -->
	c_fct(LVar, LUse, Head, LArg, LWInst1, LWInst),
	{ Term =.. [Head|LArg], LWInst1 = [fd_check_fct(Term)|LWNext] }.

last_elem(LVar, LUse, TellFdv, LWNext, LWInst) -->
	x_in_r(LVar, LUse, TellFdv, LWNext, LWInst), !.




x_in_r(LVar, LUse, I, LWNext, LWInst) -->
	var_check_type(LVar, _, fdv, I),
	terminal_check(in),
	range(LVar, LUse, Range, LWInst1, LWInst),
	{ LWInst1 = [fd_tell_range(I, Range)|LWNext] }.




range(LVar, LUse, Range, LWNext, LWInst) -->
	r_add(LVar, LUse, Range, LWInst1, LWInst),
	r_rest_add(LVar, LUse, Range, LWNext, LWInst1).

r_rest_add(LVar, LUse, Range, LWNext, LWInst) -->
	terminal(:), !,
	r_add(LVar, LUse, Range1, LWInst1, LWInst),
	{ LWInst1 = [fd_range_union(Range, Range1)|LWInst2] },
	r_rest_add(LVar, LUse, Range, LWNext, LWInst2).

r_rest_add(_, _, _, LWNext, LWNext) -->
	[].




r_add(LVar, LUse, Range, LWNext, LWInst) -->
	r_mul(LVar, LUse, Range, LWInst1, LWInst),
	r_rest_mul(LVar, LUse, Range, LWNext, LWInst1).

r_rest_mul(LVar, LUse, Range, LWNext, LWInst) -->
	terminal(&), !,
	r_mul(LVar, LUse, Range1, LWInst1, LWInst),
	{ LWInst1 = [fd_range_inter(Range, Range1)|LWInst2] },
	r_rest_mul(LVar, LUse, Range, LWNext, LWInst2).

r_rest_mul(_, _, _, LWNext, LWNext) -->
	[].


r_mul(LVar, LUse, Range, LWNext, LWInst) -->
	r_add2(LVar, LUse, Range, LWInst1, LWInst),
	r_rest_add2(LVar, LUse, Range, LWNext, LWInst1).

r_rest_add2(LVar, LUse, Range, LWNext, LWInst) -->
	terminal(Op),
	{ memberchk(Op, [++, --]) }, !,
	r_add2(LVar, LUse, Range1, LWInst1, LWInst),
	{ make_inst(Range, Op, Range1, WInst), LWInst1 = [WInst|LWInst2] },
	r_rest_add2(LVar, LUse, Range, LWNext, LWInst2).

r_rest_add2(_, _, _, LWNext, LWNext) -->
	[].



r_add2(LVar, LUse, Range, LWNext, LWInst) -->
	r_mul2(LVar, LUse, Range, LWInst1, LWInst),
	r_rest_mul2(LVar, LUse, Range, LWNext, LWInst1).

r_rest_mul2(LVar, LUse, Range, LWNext, LWInst) -->
	terminal(Op),
	{ memberchk(Op, [**, //, '%%']) }, !,
	r_mul2(LVar, LUse, Range1, LWInst1, LWInst),
	{ make_inst(Range, Op, Range1, WInst), LWInst1 = [WInst|LWInst2] },
	r_rest_mul2(LVar, LUse, Range, LWNext, LWInst2).

r_rest_mul2(_, _, _, LWNext, LWNext) -->
	[].


r_mul2(LVar, LUse, Range, LWNext, LWInst) -->
	r_prim(LVar, LUse, Range, LWInst1, LWInst),
	r_rest_prim(LVar, LUse, Range, LWNext, LWInst1).

r_rest_prim(LVar, LUse, Range, LWNext, LWInst) -->
	terminal(Op),
	{ memberchk(Op, [+, -, *, /, '%']) }, !,
	term(LVar, LUse, Term, LWInst1, LWInst),
	{ make_inst(Range, Op, Term, WInst), LWInst1 = [WInst|LWInst2] },
	r_rest_prim(LVar, LUse, Range, LWNext, LWInst2).

r_rest_prim(_, _, _, LWNext, LWNext) -->
	[].




r_prim(LVar, LUse, Range, LWNext, LWInst) -->
	terminal(~), !,
	r_prim(LVar, LUse, Range, LWInst1, LWInst),
	{ LWInst1 = [fd_range_compl(Range)|LWNext] }.

r_prim(LVar, LUse, Range, LWNext, LWInst) -->
	term(LVar, LUse, Term1, LWInst1, LWInst),
	terminal(..), !,
	term(LVar, LUse, Term2, LWInst2, LWInst1),
	{ LWInst2 = [fd_range_interval(Range, Term1, Term2)|LWNext] }.


r_prim(LVar, LUse, Range, LWNext, LWInst) -->
	terminal('{'), !,
	term_lst(LVar, LUse, LTerm, LWInst1, LWInst),
	terminal_check('}'),
	{ compile_term_lst(LTerm, Range, LWNext, LWInst1) }.

r_prim(LVar, LUse, Range, LWNext, LWInst) -->
	terminal(dom),
	terminal_check('('),
	var_check_type(LVar, V, fdv, I),
	terminal_check(')'), !,
	{ add_marked_var(LUse, V, fdv, I, dom(Range1)) },
	{ LWInst = [fd_range_copy(Range, Range1)|LWNext] }.

r_prim(LVar, LUse, Range, LWNext, LWInst) -->
	a_var(V),
	{ get_typeof(LVar, V, range, I), !, add_marked_var(LUse, V, range, I, range(Range1)) },
	{ LWInst = [fd_range_copy(Range, Range1)|LWNext] }.

r_prim(LVar, LUse, Range, LWNext, LWInst) -->
	c_fct(LVar, LUse, Head, LArg, LWInst1, LWInst), !,
	{ length(LArg, N), number_atom(N, NA), atom_concat(arg_, NA, NA1), Args =.. [NA1|LArg], LWInst1 = [fd_range_fct(Head, Range, Args)|LWNext] }.


r_prim(LVar, LUse, Range, LWNext, LWInst) -->
	terminal('('),
	range(LVar, LUse, Range, LWNext, LWInst),
	terminal(')').




term_lst(LVar, LUse, [Term|LTerm], LWNext, LWInst) -->
	term(LVar, LUse, Term, LWInst1, LWInst), !,
	term_lst_rest(LVar, LUse, LTerm, LWNext, LWInst1).

term_lst(_, _, [], LWNext, LWNext) -->
	[].


term_lst_rest(LVar, LUse, [Term|LTerm], LWNext, LWInst) -->
	terminal(','), !,
	term(LVar, LUse, Term, LWInst1, LWInst),
	term_lst_rest(LVar, LUse, LTerm, LWNext, LWInst1).

term_lst_rest(_, _, [], LWNext, LWNext) -->
	[].




compile_term_lst(LTerm, Range, LWNext, LWInst) :-
	LWInst = [fd_range_empty(Range)|LWInst1],
	compile_term_lst1(LTerm, Range, LWNext, LWInst1).




compile_term_lst1([], _, LWNext, LWNext).

compile_term_lst1([Term|LTerm], R, LWNext, [fd_range_set_value(R, Term)|LWInst]) :-
	compile_term_lst1(LTerm, R, LWNext, LWInst).





term(LVar, LUse, Term1, LWNext, LWInst) -->
	t_log(LVar, LUse, Term, LWInst1, LWInst),
	t_rest_log(LVar, LUse, Term, Term1, LWNext, LWInst1).

t_rest_log(LVar, LUse, Term1, Term4, LWNext, LWInst) -->
	terminal(Op),
	{ memberchk(Op, [&&, '||']) }, !,
	t_log(LVar, LUse, Term2, LWInst1, LWInst),
	{ make_term(Term1, Op, Term2, Term3) },
	t_rest_log(LVar, LUse, Term3, Term4, LWNext, LWInst1).

t_rest_log(_, _, Term, Term, LWNext, LWNext) -->
	[].



t_log(LVar, LUse, Term1, LWNext, LWInst) -->
	t_cmp(LVar, LUse, Term, LWInst1, LWInst),
	t_rest_cmp(LVar, LUse, Term, Term1, LWNext, LWInst1).

t_rest_cmp(LVar, LUse, Term1, Term3, LWNext, LWInst) -->
	terminal(Op),
	{ memberchk(Op, [=, ==, '!=', \=, <, <=, >, >=]) }, !,
	t_cmp(LVar, LUse, Term2, LWNext, LWInst),
	{ make_term(Term1, Op, Term2, Term3) }.

t_rest_cmp(_, _, Term, Term, LWNext, LWNext) -->
	[].




t_cmp(LVar, LUse, Term1, LWNext, LWInst) -->
	t_add(LVar, LUse, Term, LWInst1, LWInst),
	t_rest_add(LVar, LUse, Term, Term1, LWNext, LWInst1).

t_rest_add(LVar, LUse, Term1, Term4, LWNext, LWInst) -->
	terminal(Op),
	{ memberchk(Op, [+, -]) }, !,
	t_add(LVar, LUse, Term2, LWInst1, LWInst),
	{ make_term(Term1, Op, Term2, Term3) },
	t_rest_add(LVar, LUse, Term3, Term4, LWNext, LWInst1).

t_rest_add(_, _, Term, Term, LWNext, LWNext) -->
	[].



t_add(LVar, LUse, Term1, LWNext, LWInst) -->
	t_mul(LVar, LUse, Term, LWInst1, LWInst),
	t_rest_mul(LVar, LUse, Term, Term1, LWNext, LWInst1).

t_rest_mul(LVar, LUse, Term1, Term4, LWNext, LWInst) -->
	terminal(Op),
	{ memberchk(Op, [*, /<, />, mod]) }, !,
	t_mul(LVar, LUse, Term2, LWInst1, LWInst),
	{ make_term(Term1, Op, Term2, Term3) },
	t_rest_mul(LVar, LUse, Term3, Term4, LWNext, LWInst1).

t_rest_mul(_, _, Term, Term, LWNext, LWNext) -->
	[].


t_mul(LVar, LUse, - Term, LWNext, LWInst) -->
	terminal(-), !,
	t_mul(LVar, LUse, Term, LWNext, LWInst).

t_mul(_, _, Term, LWNext, LWNext) -->
	int(Term), !.

t_mul(_, _, max_integer, LWNext, LWNext) -->
	terminal(max_integer), !.

t_mul(LVar, LUse, Term, LWNext, LWNext) -->
	terminal(What),
	{ What = min ; What = max ; What = val },
	terminal_check('('),
	var_check_type(LVar, V, fdv, I),
	terminal_check(')'), !,
	{ atom_concat(What, V, Term), add_marked_var(LUse, V, fdv, I, What) }.

t_mul(LVar, LUse, Term, LWNext, LWNext) -->
	a_var(V),
	{ get_typeof(LVar, V, int, I), !, atom_concat(int, V, Term), add_marked_var(LUse, V, int, I, _) }.

t_mul(LVar, LUse, Term, LWNext, LWInst) -->
	c_fct(LVar, LUse, Head, LArg, LWNext, LWInst), !,
	{ Term =.. [Head|LArg] }.

t_mul(LVar, LUse, Term, LWNext, LWInst) -->
	terminal('('),
	term(LVar, LUse, Term, LWNext, LWInst),
	terminal(')').




c_fct(_, _, Head, [], LWNext, LWNext) -->
	ident(Head),
	terminal('('),
	terminal(')'), !.

c_fct(LVar, LUse, Head, LArg, LWNext, LWInst) -->
	ident(Head),
	terminal('('),
	parm_lst(LVar, LUse, LArg, LWNext, LWInst),
	terminal_check(')'), !.




parm_lst(LVar, LUse, [Arg|LArg], LWNext, LWInst) -->
	parm_one(LVar, LUse, Arg, LWInst1, LWInst), !,
	parm_rest_lst(LVar, LUse, LArg, LWNext, LWInst1).

parm_lst(_, _, _, _, _) -->
	syn_error('C fct parameter').


parm_rest_lst(LVar, LUse, LArg, LWNext, LWInst) -->
	terminal(','), !,
	parm_lst(LVar, LUse, LArg, LWNext, LWInst).

parm_rest_lst(_, _, [], LWNext, LWNext) -->
	[].




parm_one(LVar, LUse, Arg, LWNext, LWInst) -->
	term(LVar, LUse, Arg, LWNext, LWInst),
	(   terminal(..) ->
	    { fail }
	;   { true }
	).

parm_one(LVar, LUse, range_arg(Range), LWNext, LWInst) -->
	range(LVar, LUse, Range, LWNext, LWInst).

parm_one(LVar, LUse, Arg, LWNext, LWNext) -->
	a_var(V),                                                 % local var
	{ get_typeof(LVar, V, T, I), atom_concat(T, V, Arg), add_marked_var(LUse, V, T, I, _) }.

parm_one(LVar, LUse, &(Arg), LWNext, LWInst) -->
	terminal(&),                                                 % adr of
	parm_one(LVar, LUse, Arg, LWNext, LWInst).




var_check_type(LVar, V, T, I) -->
	a_var(V),
	(   { get_typeof(LVar, V, T1, I) } ->
	    (   { T = T1 } ->
	        { true }
	    ;   sem_error('variable ~a declared as ~a used as ~a', [V, T1, T])
	    )
	;   sem_error('undeclared variable ~a', [V])
	).





var_check_new(LVar, V) -->
	a_var(V),
	(   { get_typeof(LVar, V, T, _) } ->
	    sem_error('variable ~a already declared as ~a', [V, T])
	;   { true }
	).




a_var(V) -->
	ident(V).


ident(X) -->
	terminal(ident(X)).


ident_check(X) -->
	ident(X), !.

ident_check(_) -->
	syn_error(identifier).


int(X) -->
	terminal(X),
	{ integer(X) }.




terminal(X) -->
	[t(X, _, _)].

terminal_check(X) -->
	terminal(X), !.

terminal_check(X) -->
	syn_error(X).


syn_error(Expected) -->
	[t(T, L, C)],
	{ error('~d: syntax error : ~w expected at "~w" (char:~d)', [L, Expected, T, C]) }.


sem_error(Msg, Args) -->
	[t(_, L, C)],
	{ append([L, Msg|Args], [C], M), error('~d: ~? (char:~d)', M) }.




          % Utilities


add_use_to_dep(LUse, _) :-
	var(LUse), !.

add_use_to_dep([Use|LUse], LDep) :-
	arg(2, Use, T),
	(   T \== fdv,
	    T \== l_fdv ->
	    true
	;   member(Use, LDep)
	), !,
	add_use_to_dep(LUse, LDep).




make_term(T1, Op, T2, T3) :-
	convert(Op, Op1), !,
	functor(T3, Op1, 2),
	arg(1, T3, T1),
	arg(2, T3, T2).




convert(/<, 'DivDn').
convert(/>, 'DivUp').
convert(mod, '%').
convert(=, ==).
convert(\=, '!=').
convert(X, X).




make_inst(Arg1, Op, Arg2, WInst) :-
	inst_name(Op, F), !,
	functor(WInst, F, 2),
	arg(1, WInst, Arg1),
	arg(2, WInst, Arg2).



inst_name(++, fd_range_add_range).
inst_name(--, fd_range_sub_range).
inst_name(**, fd_range_mul_range).
inst_name(//, fd_range_div_range).
inst_name('%%', fd_range_mod_range).
inst_name(+, fd_range_add_value).
inst_name(-, fd_range_sub_value).
inst_name(*, fd_range_mul_value).
inst_name(/, fd_range_div_value).
inst_name('%', fd_range_mod_value).




get_typeof(LVar, V, T, I) :-
	memb(v(V, T, I), LVar).




memb(X, Y) :-
	nonvar(Y),
	Y = [H|T],
	(   H = X, !
	;   memb(X, T)
	).




add_local_var(LVar, Decl) :-
	member(Decl, LVar), !.




add_marked_var([m(V, T, I, Mark)|_], V, T, I, What) :-
	!,
	(   (   T = fdv
	    ;   T = l_fdv
	    ),
	    nonvar(What) ->
	    (   What = min,
	        Mark = i(t, _, _, _)
	    ;   What = max,
	        Mark = i(_, t, _, _)
	    ;   What = dom(R),
	        Mark = i(_, _, t(R), _)
	    ;   What = val,
	        Mark = i(_, _, _, t)
	    ), !
	;   Mark = What
	).


add_marked_var([_|LUse], V, T, I, What) :-
	add_marked_var(LUse, V, T, I, What).




close_list([]) :-
	!.

close_list([_|L]) :-
	close_list(L).
