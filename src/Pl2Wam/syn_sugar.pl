/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog to WAM compiler                                          *
 * File  : syn_sugar.pl                                                    *
 * Descr.: pass 1: syntactic sugar removing                                *
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



syntactic_sugar_init_pred(Pred, _) :-
	'$aux_name'(Pred), !.

syntactic_sugar_init_pred(_, _) :-
	g_assign(aux, 1).




syntactic_sugar(SrcCl, Head, Body2) :-
	(   SrcCl = (Head :- Body)
	;   SrcCl = Head,
	    Body = true
	), !,
	normalize_cuts(Body, Body1),
	normalize_alts(Body1, Head, Body2).



normalize_cuts(Body, Body2) :-
	g_assign(has_cut, f),
	normalize_cuts1(Body, CutVar, Body1), !,
	(   g_read(has_cut, t) ->
	    Body2 = ('$get_cut_level'(CutVar), Body1)
	;   Body2 = Body1
	).


	/* NB: difference between cut and soft cut
   	 * cut: a cut generates a '$get_cut_level'(V) followed by a '$cut'(V)
	 *      '$get_cut_level'(V) will give rise to a copy from X(arity) to V
	 *      (see code_gen.pl). The initialization of X(arity) is done once
	 *      at the beginning of the predicate (before any choice-point creation)
	 *      thus X(arity) is saved in choice-points. The initialization is
	 *      done by a WAM instruction get_current_choice(x(Arity)) (indexing.pl)
	 *
	 * soft cut: give rise to a '$get_current_choice'(V) followed by a '$cut'(V)
	 *      '$get_current_choice'(V) is translated as a WAM instruction
	 *      get_current_choice(V).
	 *
	 * Thus, a cut points to the last choice-point to keep while a soft cut
	 * points to the choice-point to kill.
	 */

normalize_cuts1(X, CutVar, P) :-
	var(X),
	normalize_cuts1(call(X), CutVar, P).

normalize_cuts1((If ; R), CutVar, Body) :-
	nonvar(If),
	(   If = (P -> Q), Body = ('$get_cut_level'(CutVar1), P, '$cut'(CutVar1), Q1 ; R1)
	;
	    If = (P *-> Q), Body = ('$get_current_choice'(CutVar1), P, '$soft_cut'(CutVar1), Q1 ; R1)
	),
	normalize_cuts1(Q, CutVar, Q1),
	normalize_cuts1(R, CutVar, R1).

normalize_cuts1((P -> Q), CutVar, Body) :-
	Body = ('$get_cut_level'(CutVar1), P, '$cut'(CutVar1), Q1 ; fail),
	normalize_cuts1(Q, CutVar, Q1).

	% P *-> Q alone (i.e. not inside a ;) is logically the same as P, Q. 
	% However a cut in the test part (P) should be local to P (as in P -> Q).
	% Hence we replace P *-> Q by P *-> Q1 ; fail

normalize_cuts1((P *-> Q), CutVar, ((P, Q1) ; fail)) :-
	normalize_cuts1(Q, CutVar, Q1).

normalize_cuts1(!, CutVar, '$cut'(CutVar)) :-
	g_assign(has_cut, t).

normalize_cuts1((P, Q), CutVar, (P1, Q1)) :-
	normalize_cuts1(P, CutVar, P1),
	normalize_cuts1(Q, CutVar, Q1).

normalize_cuts1((P ; Q), CutVar, (P1 ; Q1)) :-
	normalize_cuts1(P, CutVar, P1),
	normalize_cuts1(Q, CutVar, Q1).

normalize_cuts1(M:G, CutVar, Body) :-
	check_module_name(M, true),
	normalize_cuts1(G, CutVar, G1),
	distrib_module_qualif(G1, M, G2),
	(   G2 = M3:_, var(M3) ->
	    normalize_cuts1(call(G2), CutVar, Body)
	;
	    Body = G2
	).

normalize_cuts1(call(G), _, '$call'(G, Func, Arity, true)) :-
%	get_module_of_cur_pred(Module), % then use a '$call'(G, Module, Func, Arity, true)
	cur_pred_without_aux(Func, Arity).

normalize_cuts1(catch(G, C, R), _, '$catch'(G, C, R, Func, Arity, true)) :-
	cur_pred_without_aux(Func, Arity).

normalize_cuts1(throw(B), _, '$throw'(B, Func, Arity, true)) :-
	cur_pred_without_aux(Func, Arity).

normalize_cuts1(P, _, P1) :-
	(   callable(P) ->
	    meta_pred_rewriting(P, P1)
	;   error('body goal is not callable (~q)', [P])
	).




distrib_module_qualif((P ; Q), M, (P1 ; Q1)) :-
	!,
	distrib_module_qualif(P, M, P1),
	distrib_module_qualif(Q, M, Q1).

distrib_module_qualif((P -> Q), M, (P1 -> Q1)) :-
	!,
	distrib_module_qualif(P, M, P1),
	distrib_module_qualif(Q, M, Q1).

distrib_module_qualif((P , Q), M, (P1 , Q1)) :-
	!,
	distrib_module_qualif(P, M, P1),
	distrib_module_qualif(Q, M, Q1).

distrib_module_qualif(M:G, _, G1) :-
	!,
	check_module_name(M, true),
	distrib_module_qualif(G, M, G1).

distrib_module_qualif(!, _, !) :-
	!.

distrib_module_qualif(P, M, M:P).

distrib_module_qualif_goal(G, _, G) :-
	nonvar(G),
	G = M:_, !,		% already qualifed with a module
	check_module_name(M, true).

distrib_module_qualif_goal(G, M, M:G).




normalize_alts(Body, Head, Body1) :-
	functor(Head, Pred, N),
	g_assign(head_functor, Pred),
	g_assign(head_arity, N),
	normalize_alts1(Body, Head, Body1), !.


normalize_alts1(X, _, call(X)) :-
	var(X).

normalize_alts1((P, Q), RestC, (P1, Q1)) :-
	normalize_alts1(P, (RestC, Q), P1),
	normalize_alts1(Q, (RestC, P), Q1).

normalize_alts1((P ; Q), RestC, AuxPred) :-
	lst_var(RestC, [], VarRestC),
	lst_var((P ; Q), [], VarAlt),
	set_inter(VarAlt, VarRestC, V),
	length(V, N1),
	g_read(head_functor, Pred),
	g_read(head_arity, N),
	init_aux_pred_name(Pred, N, AuxName, N1),
	AuxPred =.. [AuxName|V],
	g_read(where, Where),
	linearize((P ; Q), AuxPred, Where, LAuxSrcCl),
	asserta(buff_aux_pred(AuxName, N1, LAuxSrcCl)).

normalize_alts1(P, _, P1) :-
	pred_rewriting(P, P1), !.




init_aux_pred_name(Pred, N, AuxName, N1) :-
	g_read(aux, Aux),
	Aux1 is Aux + 1,
	g_assign(aux, Aux1),
	'$make_aux_name'(Pred, N, Aux, AuxName),
	(   test_pred_info(bpl, Pred, N),
	    set_pred_info(bpl, AuxName, N1)
	;   test_pred_info(bfd, Pred, N),
	    set_pred_info(bfd, AuxName, N1)
	;   true
	), !.




linearize(Body, AuxPred, Where, LAuxSrcCl) :-
	(   Body = (P ; Q) ->
	    linearize(Q, AuxPred, Where, LAuxSrcCl1),
	    linearize1(P, AuxPred, Where, LAuxSrcCl2),
	    append(LAuxSrcCl2, LAuxSrcCl1, LAuxSrcCl)
	;   linearize1(Body, AuxPred, Where, LAuxSrcCl)
	).


linearize1(fail, _, _, []) :-
	!.

linearize1(P, AuxPred, Where, [Where + AltP]) :-
	copy_term((AuxPred :- P), AltP).




lst_var(X, V, V1) :-
	var(X), !,
	set_add(V, X, V1).

lst_var(P, V, V1) :-
	functor(P, _, N),
	lst_var_args(1, N, P, V, V1).



lst_var_args(I, N, P, V, V2) :-
	(   I =< N ->
	    arg(I, P, ArgP),
	    lst_var(ArgP, V, V1),
	    I1 is I + 1,
	    lst_var_args(I1, N, P, V1, V2)
	;   V2 = V
	).




        % Other predicate rewriting

pred_rewriting(fd_tell(X), T) :-                          % FD transformation
	test_c_call_allowed(fd_tell / 1),
	pred_rewriting('$call_c'(X, [boolean]), T).

pred_rewriting(set_bip_name(Name, Arity), Pred1) :-
	g_read(inline, t),      % also if byte code since implies --no-inline
	(   atom(Name), integer(Arity) ->
	    CallC = '$call_c'('Pl_Set_Bip_Name_Untagged_2'(Name, Arity), [by_value])
	;
	    CallC = '$call_c'('Pl_Set_Bip_Name_2'(Name, Arity))
	),
	pred_rewriting(CallC, Pred1).

pred_rewriting(Pred, Pred1) :-                     % math define current bip
	g_read(inline, t),      % also if byte code since implies --no-inline
	g_read(fast_math, f),
	functor(Pred, F, 2),
	(   F = (is)
	;   math_cmp_functor_name(F, _)
	),					            % see code_gen.pl
	pred_rewriting(set_bip_name(F, 2), T),
	Pred1 = (T, Pred).

pred_rewriting(term_hash(Term, Hash), Pred1) :-
	g_read(inline, t),      % also if byte code since implies --no-inline
	catch(term_hash(Term, Hash1), _, fail), % if there is an error, do not inline !
	integer(Hash1), !,
	pred_rewriting(set_bip_name(term_hash, 2), T),
	pred_rewriting('$call_c'('Pl_Un_Integer_Check'(Hash1, Hash), [boolean]), T2),
	Pred1 = (T, T2).

pred_rewriting(term_hash(Term, Depth, Range, Hash), Pred1) :-
	g_read(inline, t),      % also if byte code since implies --no-inline
	catch(term_hash(Term, Depth, Range, Hash1), _, fail), % if there is an error, do not inline !
	integer(Hash1), !,
	pred_rewriting(set_bip_name(term_hash, 4), T),
	pred_rewriting('$call_c'('Pl_Un_Integer_Check'(Hash1, Hash), [boolean, by_value]), T2),
	Pred1 = (T, T2).

          % The user should use: '$call_c'(F) or '$call_c'(F, LCOpt)
          % LCOpt is a list containing:
          %   jump/boolean
          %   fast_call (use a fact call convention)
          %   tagged (use tagged calls for atom, integers and F/N)
          %   by_value (pass atom, numbers, F/N by value not by WamWord)
          %   use_x_regs (the function can destroy any X register)
          %
          % Backward compatibility: '$call_c_test'/1 and '$call_c_jump'/1 are
          % kept for the moment...

          % do not use LCOpt1 = '$no_internal_transf$'(LCOpt) for bootstrapping
pred_rewriting('$call_c'(F, LCOpt), '$call_c'(F, LCOpt1)) :-
	test_c_call_allowed('$call_c' / 2),
	no_internal_transf(LCOpt, LCOpt1).

pred_rewriting('$call_c'(F), T) :-
	test_c_call_allowed('$call_c' / 1),
	pred_rewriting('$call_c'(F, []), T).

						% backward compatibility
pred_rewriting('$call_c_test'(F), T) :-
	test_c_call_allowed('$call_c_test' / 1),
	pred_rewriting('$call_c'(F, [boolean]), T).

						% backward compatibility
pred_rewriting('$call_c_jump'(F), T) :-
	test_c_call_allowed('$call_c_jump' / 1),
	pred_rewriting('$call_c'(F, [jump]), T).

pred_rewriting(P, P).



test_c_call_allowed(_) :-
	g_read(call_c, t), !.

test_c_call_allowed(X) :-
	error('~q not allowed in this mode', [X]).



          % can be considered as inline (neither CP nor X regs to save)

not_dangerous_c_call([]).

not_dangerous_c_call([COpt|LCOpt]) :-
	COpt \== jump,
	COpt \== use_x_regs,
	not_dangerous_c_call(LCOpt).




add_wrapper_to_dyn_clause(Pred, N, Where + Cl, AuxName) :-
	init_aux_pred_name(Pred, N, AuxName, N),
	(   Cl = (Head :- Body) ->
	    head_wrapper(Head, AuxName, Head1),
	    Cl1 = (Head1 :- Body)
	;   head_wrapper(Cl, AuxName, Cl1)
	),
	assertz(buff_aux_pred(AuxName, N, [Where + Cl1])).


head_wrapper(Head, AuxName, Head1) :-
	Head =.. [_|LArgs],
	Head1 =.. [AuxName|LArgs].



	% meta_predicate rewriting

meta_pred_rewriting(P1, P2) :-
	nonvar(P1),
	functor(P1, Pred, N),
	clause(meta_pred(Pred, N, MetaDecl), _), !,
	functor(P2, Pred, N),
	meta_pred_rewrite_args(1, N, P1, MetaDecl, P2).

meta_pred_rewriting(P, P).




meta_pred_rewrite_args(I, N, P1, MetaDecl, P2) :-
	I =< N, !,
	arg(I, P1, A1),
	arg(I, P2, A2),
	arg(I, MetaDecl, Spec),
	meta_pred_rewrite_arg(Spec, A1, A2),
	I1 is I + 1,
	meta_pred_rewrite_args(I1, N, P1, MetaDecl, P2).

meta_pred_rewrite_args(_, _, _, _, _).




meta_pred_rewrite_arg(Spec, A, A) :-
	var(Spec), !.

meta_pred_rewrite_arg(:, A1, A2) :-
	!,
	meta_pred_rewrite_arg(0, A1, A2).

meta_pred_rewrite_arg(Spec, A1, '$mt'(Module, A1)) :-
	integer(Spec), !,
	get_module_of_cur_pred(Module).

meta_pred_rewrite_arg(_, A, A).
