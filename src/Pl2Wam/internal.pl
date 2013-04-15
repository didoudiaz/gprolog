/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog to WAM compiler                                          *
 * File  : internal.pl                                                     *
 * Descr.: pass 2: internal format transformation                          *
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
 * predicate internal format: (I(t)=internal format of t)                  *
 *                                                                         *
 * I(p(Arg1,..., ArgN)): p(NoPred, Module, Pred/N, [I(Arg1), ..., I(ArgN)])*
 *                                                                         *
 * NoPred : predicate number = corresponding chunk number                  *
 *                                                                         *
 * Module: module qualification or module owned if export (a variable else)*
 *                                                                         *
 * Pred/N : predicate/arity                                                *
 *                                                                         *
 * I(Argi): internal format of the ith argument                            *
 *                                                                         *
 *    var : var(VarName, Info) with:                                       *
 *          VarName = x(NoX) temporary                                     *
 *                    (here NoX is unbound or = void if var is singleton)  *
 *                    y(NoY) permanent (NoY is assigned here)              *
 *          Info    = in_heap       : the var is stored in the heap        *
 *                    unsafe        : the var refers current environment   *
 *                    not_in_cur_env: the var does not reside in the       *
 *                                    current environment                  *
 *          (here Info is unbound)                                         *
 *                                                                         *
 *    atom []       : nil                                                  *
 *    atom (others) : atm(atom)                                            *
 *    integer       : int(integer)                                         *
 *    float         : flt(float)                                           *
 *    f(A1, ..., An): stc(f, n, [I(A1), ..., I(An)])  ([H|T] = '.'(H, T))  *
 *                                                                         *
 * NB: a true/0 in the body of a clause is removed.                        *
 *     variables are classified and permanent variables are assigned       *
 *     (temporary = x(_), permanent = y(i))                                *
 *-------------------------------------------------------------------------*/

internal_format(Head, Body, Head1, Body1, NbChunk, NbY) :-
	format_head(Head, DicoVar, Head1),
	format_body(Body, DicoVar, Body1, NbChunk),
	classif_vars(DicoVar, 0, NbY).




format_head(Head, DicoVar, Head1) :-
	g_read(module, Module),	% not really necessary since Module info in p(...) is never used
	format_pred(Module:Head, 0, DicoVar, Head1, _).




format_body(Body, DicoVar, Body1, NbChunk) :-
	format_body1(Body, 0, DicoVar, t, [], Body1, NbChunk, _).

format_body1((P, Q), NoPred, DicoVar, StartChunk, LNext, P1, NoPred2, StartChunk2) :-
	!,
	format_body1(P, NoPred, DicoVar, StartChunk, Q1, P1, NoPred1, StartChunk1),
	format_body1(Q, NoPred1, DicoVar, StartChunk1, LNext, Q1, NoPred2, StartChunk2).

format_body1(true, NoPred, _, StartChunk, LNext, LNext, NoPred, StartChunk) :-
	!.

format_body1(Pred, NoPred, DicoVar, StartChunk, LNext, [Pred1|LNext], NoPred1, StartChunk1) :-
	(   StartChunk = t ->
	    NoPred1 is NoPred + 1
	;   NoPred1 = NoPred
	),
	format_pred(Pred, NoPred1, DicoVar, Pred1, InlinePred),
	(   InlinePred = t ->
	    StartChunk1 = f
	;   StartChunk1 = t
	).



          % NB: a dangerous '$call_c' (e.g. with jump) is not considered as
          % inlined to enforce the end of its chunk. If something comes
          % after this '$call_c' an environment will be created (allocate)
          % to save CP (and X regs in Y regs if needed).
          % Other '$call_c' are considered as inlined.

format_pred(Module:Pred, NoPred, DicoVar, p(NoPred, Module, FN, ArgLst), InlinePred) :-
	!,
	format_pred(Pred, NoPred, DicoVar, p(NoPred, _, FN, ArgLst), InlinePred).

format_pred(Pred, NoPred, DicoVar, p(NoPred, Module, F / N, ArgLst1), InlinePred) :-
	functor(Pred, F, N),
	get_owner_module(F, N, Module),
	Pred =.. [_|ArgLst],
	format_arg_lst(ArgLst, NoPred, DicoVar, ArgLst1),
	(   (   inline_predicate(F, N)
            ;   F = '$call_c',
	        N = 2,
		ArgLst1 = [_, LCOpt],  % $no_internal_transf$ removed here
	        not_dangerous_c_call(LCOpt)
	    ) ->
	    InlinePred = t
	;   InlinePred = f
	).




format_arg_lst([], _, _, []).

format_arg_lst([Arg|ArgLst], NoPred, DicoVar, [Arg1|ArgLst1]) :-
	format_arg(Arg, NoPred, DicoVar, Arg1), !,
	format_arg_lst(ArgLst, NoPred, DicoVar, ArgLst1).




format_arg(Var, NoPred, DicoVar, V) :-
	var(Var),
	add_var_to_dico(DicoVar, Var, NoPred, V).


format_arg(T, _, _, T1) :-
	no_internal_transf(T1, T).

format_arg([], _, _, nil).

format_arg(A, _, _, atm(A)) :-
	atom(A).

format_arg(N, _, _, int(N)) :-
	integer(N).

format_arg(N, _, _, flt(N)) :-
	float(N).

format_arg(Fonc, NoPred, DicoVar, stc(F, N, ArgLst1)) :-
	functor(Fonc, F, N),
	Fonc =.. [_|ArgLst],
	format_arg_lst(ArgLst, NoPred, DicoVar, ArgLst1).




          % creates a term T1 equivalent to T which will not be transformed
          % in the internal format. This can only by used for arguments of
          % inlined predicates and requires T is ground.
          %
          % NB: do not use T1 = '$no_internal_transf$'(T) for bootstrapping.

no_internal_transf(T, T1) :-
	functor(T1, '$no_internal_tranf$', 1),
	arg(1, T1, T).




          % DicoVar=[ v(Var, NoPred1stOcc, Singleton, V), ... | EndVar ]
          %
          % Singleton = f or unbound variable
          % V = var(VarName, VarInfo)
          % VarName = x(_) or y(_)
          % Info is unbound

add_var_to_dico(DicoVar, Var, NoPred1stOcc, V) :-
	var(DicoVar), !,
	V = var(_, _),
	DicoVar = [v(Var, NoPred1stOcc, _, V)|_].

add_var_to_dico([v(Var1, NoPred1stOcc1, Singleton, V)|_], Var2, NoPred1stOcc2, V) :-
	Var1 == Var2, !,
	V = var(VarName, _),
	Singleton = f,
	(   var(VarName),
	    NoPred1stOcc1 \== NoPred1stOcc2,
	    NoPred1stOcc2 > 1 ->
	    VarName = y(_)
	;   true
	).

add_var_to_dico([_|DicoVar], Var, NoPred1stOcc, V) :-
	add_var_to_dico(DicoVar, Var, NoPred1stOcc, V).




classif_vars([], NbY, NbY) :-
	!.

classif_vars([v(_, _, Singleton, var(VarName, _))|DicoVar], Y, NbY) :-
	var(VarName), !,
	(   var(Singleton) ->
	    VarName = x(void)
	;   VarName = x(_)
	),
	classif_vars(DicoVar, Y, NbY).

classif_vars([v(_, _, _, var(y(Y), _))|DicoVar], Y, NbY) :-
	Y1 is Y + 1,
	classif_vars(DicoVar, Y1, NbY).




	% Inline predicates: inline_predicate(Pred,Arity)
	% all predicates defined here must have a corresponding clause
	% gen_inline_pred/5 in pass 3 describing their associated code

inline_predicate(Pred, Arity) :-
	g_read(inline, Inline),
	inline_predicate(Pred, Arity, Inline).




inline_predicate('$get_cut_level', 1, _).

inline_predicate('$get_current_choice', 1, _).

inline_predicate('$cut', 1, _).

inline_predicate('$soft_cut', 1, _).




inline_predicate(=, 2, _).

inline_predicate('$foreign_call_c', 1, _).


inline_predicate(var, 1, t).

inline_predicate(nonvar, 1, t).

inline_predicate(atom, 1, t).

inline_predicate(integer, 1, t).

inline_predicate(float, 1, t).

inline_predicate(number, 1, t).

inline_predicate(atomic, 1, t).

inline_predicate(compound, 1, t).

inline_predicate(callable, 1, t).

inline_predicate(ground, 1, t).

inline_predicate(is_list, 1, t).

inline_predicate(list, 1, t).

inline_predicate(partial_list, 1, t).

inline_predicate(list_or_partial_list, 1, t).


inline_predicate(fd_var, 1, t).

inline_predicate(non_fd_var, 1, t).

inline_predicate(generic_var, 1, t).

inline_predicate(non_generic_var, 1, t).




inline_predicate(functor, 3, t).

inline_predicate(arg, 3, t).

inline_predicate(compare, 3, t).

inline_predicate(=.., 2, t).



inline_predicate(==, 2, t).

inline_predicate(\==, 2, t).

inline_predicate(@<, 2, t).

inline_predicate(@=<, 2, t).

inline_predicate(@>, 2, t).

inline_predicate(@>=, 2, t).




inline_predicate(is, 2, t).

inline_predicate(=:=, 2, t).

inline_predicate(=\=, 2, t).

inline_predicate(<, 2, t).

inline_predicate(=<, 2, t).

inline_predicate(>, 2, t).

inline_predicate(>=, 2, t).




inline_predicate(g_assign, 2, t).

inline_predicate(g_assignb, 2, t).

inline_predicate(g_link, 2, t).

inline_predicate(g_read, 2, t).

inline_predicate(g_array_size, 2, t).

inline_predicate(g_inc, 1, t).

inline_predicate(g_inco, 2, t).

inline_predicate(g_inc, 2, t).

inline_predicate(g_inc, 3, t).

inline_predicate(g_dec, 1, t).

inline_predicate(g_deco, 2, t).

inline_predicate(g_dec, 2, t).

inline_predicate(g_dec, 3, t).

inline_predicate(g_set_bit, 2, t).

inline_predicate(g_reset_bit, 2, t).

inline_predicate(g_test_set_bit, 2, t).

inline_predicate(g_test_reset_bit, 2, t).

