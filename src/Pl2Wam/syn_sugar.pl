/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog to WAM compiler                                          *
 * File  : syn_sugar.pl                                                    *
 * Descr.: pass 1: syntactic sugar removing                                *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2021 Daniel Diaz                                     *
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


    /* for auxiliary predicates we do not restart the aux counter
     * but instead we continue sequentially.
     * All aux predicates stemming from p/n have the same prefix
     * (father pred p/n).
     */
  
syntactic_sugar_init_pred(Pred, _, _) :-
	'$aux_name'(Pred), !.

    /* Caution: the aux predicates stemming from a multifile pred p/n can
     * cause name clashes when compiled to byte-code.
     * These aux pred names are named p/n_$aux<K> where K is a seq number.
     * Since these predicates are also stored in the global predicate table
     * 2 clauses for p/n (defined in p1.pl and p2.pl) giving rise to aux
     * predicates will produce 2 clasinhg p/n_$aux1.
     * We here use the hash of the file name and a random number for the
     * starting aux number.
     */

syntactic_sugar_init_pred(Pred, N, PlFile) :-
	(   g_read(native_code, f), test_pred_info(multi, Pred, N) ->
	    randomize,
	    term_hash(PlFile, H),
	    Max is (1 << 26),
	    random(1, Max, R),
	    Aux is (H + R) /\ (Max - 1) % avoid negative number
	;
	    Aux = 1
	),
	g_assign(aux, Aux).




syntactic_sugar(SrcCl, Head, Body2) :-
	(   SrcCl = (Head :- Body)
	;   SrcCl = Head,
	    Body = true
	), !,
	normalize_cuts(Body, Body1, _HasCut),
	normalize_alts(Body1, Head, Body2).


 /*
  * Cuts
  *
  * The compilation of cut in p/n requires to:
  *    1) copy the value of B at the entry of the p/n
  *    2) restore B with this copy when the cut ! occurs
  *
  * In 1), the instruction to copy B must occur before the code of any clause
  * (i.e. before any choice-point creation of the predicate).
  * Here, we generate a '$get_cut_level'(V) at the beginning of a clause
  * contianing cuts and a '$cut'(V) for each effective cut.
  *
  * If p/n (with args in X(0)..X(n-1)) has cuts, B is copied into X(n)
  * with a get_current_choice(X(n)) instruction. This must occur before any
  * choice-point creation instruction (in indexing.pl).
  * NB: if X(n) is used for the cut level, it must be saved in choice-points
  * in the case of a cut occurs in a following clause. For this we generate a
  * pragma_arity(N+1) which adjust the number of arguments to save in the
  * choice-points (indexing.pl).
  *
  * A '$get_cut_level'(V) will give rise to a simple copy instruction X(n)->V
  * (in code_gen.pl).
  * A '$cut'(V) will produce a cut(V) WAM instruction.
  * 
  *
  * If-Then/If-Then-Else
  *
  * (If -> Then) is rewritten as
  *   '$get_current_choice'(X), If, '$cut'(X).
  *
  * (If -> Then ; Else) is rewritten as
  *   ('$get_cut_level'(X), If, '$cut'(X), Then ; Else)
  * which will give rise to an auxiliary predicate:
  *
  *   'p/n_$auxK' :- '$get_cut_level'(X), If, '$cut'(X), Then.
  *   'p/n_$auxK' :- Else.
  * 
  * Soft-Cut
  *
  * A soft-cut preserve the alternatives of If and only "cut" the Else
  * alternative. For this the soft_cut(B') WAM instruction "forgets" the
  * choice-point pointed by B' (choice-points are traversed from the top B
  * until B' is encountered ; it is then unlinked).
  * While a cut instruction points to the last choice-point to keep) a
  * a  soft-cut instruction points to the choice-point to kill.
  *
  * (If *-> Then) is equivalent to and rewritten as
  *   (If, Then)
  *
  * (If *-> Then ; Else) is rewritten as
  *   '$get_current_choice'(X), If, '$soft_cut'(X), Then ; Else
  * which will give rise to an auxiliary predicate:
  *   'p/n_$auxK' :- '$get_current_choice'(X), If, '$soft_cut'(X), Then
  *   'p/n_$auxK' :- Else.
  *
  * Cut opacity:
  *
  * In -> or *->, the If part is not transparent to cut (ie. opaque). A cut in
  * the If part is thus local to the If (think to a call(If) instead of If).
  * e.g. (If -> Then ; Else)  <==>  (call(If) -> Then ; Else)
  *
  * If a cut occurs in If, '$cut'(X) is produced to restore B to the
  * choice point recorded just before the If. Cuts in Then or Else give rise
  * to a classical '$cut'(V) as explained at the beginning.
  */

normalize_cuts(Body, Body2, HasCut) :-
	normalize_cuts1(Body, CutVar, Body1, HasCut), !,
	(   HasCut == t ->
	    Body2 = ('$get_cut_level'(CutVar), Body1)
	;
	    Body2 = Body1
	).




normalize_cuts1(P, CutVar, P1, HasCut) :-
	var(P),
	normalize_cuts1(call(P), CutVar, P1, HasCut).

normalize_cuts1(!, CutVar, '$cut'(CutVar), t).

normalize_cuts1((IfThen ; R), CutVar, Body, HasCut) :-
	nonvar(IfThen),
	(   IfThen = (P -> Q),
	    Body1 = ('$get_cut_level'(CutVar1), P1, '$cut'(CutVar1), Q1 ; R1)
	;
	    IfThen = (P *-> Q),
	    Body1 = ('$get_current_choice'(CutVar1), P1, '$soft_cut'(CutVar1), Q1 ; R1)
	),
	normalize_cuts1(R, CutVar, R1, HasCut),
	(   g_read(optim_fail, t), R1 == fail ->
	    normalize_cuts1(IfThen, CutVar, Body, HasCut)
	;
	    normalize_cuts_in_if(P, P1),
	    normalize_cuts1(Q, CutVar, Q1, HasCut),
	    Body = Body1
	).

normalize_cuts1((P -> Q), CutVar, Body, HasCut) :-
	normalize_cuts1(P, CutVar1, P1, _HasCut1),
	normalize_cuts1(Q, CutVar, Q1, HasCut),
	Body = ('$get_current_choice'(CutVar1), P1, '$cut'(CutVar1), Q1).

	% P *-> Q alone (i.e. not inside a ;) is logically the same as P, Q. 
normalize_cuts1((P *-> Q), CutVar, (P1, Q1), HasCut) :-
	normalize_cuts_in_if(P, P1),
	normalize_cuts1(Q, CutVar, Q1, HasCut).

normalize_cuts1((P ; Q), CutVar, Body, HasCut) :-
	normalize_cuts1(P, CutVar, P1, HasCut),
	normalize_cuts1(Q, CutVar, Q1, HasCut),
	(   g_read(optim_fail, t), P1 == fail, Body = Q1
	;
	    g_read(optim_fail, t), Q1 == fail, Body = P1
	;
	    Body = (P1; Q1)
	).
	    
normalize_cuts1((P, Q), CutVar, (P1, Q1), HasCut) :-
	normalize_cuts1(P, CutVar, P1, HasCut),
	normalize_cuts1(Q, CutVar, Q1, HasCut).

normalize_cuts1(Module:G, CutVar, Body, HasCut) :-
	check_module_name(Module, true),
	normalize_cuts1(G, CutVar, G1, HasCut),
	distrib_module_qualif(G1, Module, G2),
	(   G2 = M2:_, var(M2) ->
	    normalize_cuts1(call(G2), CutVar, Body, _)
	;
	    Body = G2
	).

normalize_cuts1(call(G), _, '$call'(G, Func, Arity, true), _HasCut) :-
%	get_module_of_cur_pred(Module), % then use a '$call'(G, Module, Func, Arity, true)
	cur_pred_without_aux(Func, Arity).

normalize_cuts1(catch(G, C, R), _, '$catch'(G, C, R, Func, Arity, true), _HasCut) :-
	cur_pred_without_aux(Func, Arity).

normalize_cuts1(throw(B), _, '$throw'(B, Func, Arity, true), _HasCut) :-
	cur_pred_without_aux(Func, Arity).

normalize_cuts1(P, _, P1, _HasCut) :-
	(   callable(P) ->
	    meta_pred_rewriting(P, P1)
	;
	    error('body goal is not callable (~q)', [P])
	).




	/* A cut in the if-part is local (if-part is opaque)
	 * If a cut appears we have to get the current choice point
	 * at the entry of the if-part and use it for cuts in the if-part.
	 */

normalize_cuts_in_if(P, Body) :-
	normalize_cuts1(P, CutVar, P1, HasCut),
	(   HasCut == t ->
	    Body = ('$get_current_choice'(CutVar), P1)
	;
	    Body = P1
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

normalize_alts1(Body, RestC, AuxPred) :-
	functor(Body, ';', 2),
	lst_var(RestC, [], VarRestC),
	lst_var(Body, [], VarAlt),
	set_inter(VarAlt, VarRestC, V),
	length(V, AuxN),
	g_read(head_functor, Pred),
	g_read(head_arity, N),
	init_aux_pred_name(Pred, N, AuxName, AuxN),
	AuxPred =.. [AuxName|V],
	g_read(where, Where),
	linearize(Body, AuxPred, Where, LAuxSrcCl),
	asserta(buff_aux_pred(AuxName, AuxN, LAuxSrcCl)).

normalize_alts1(P, _, P1) :-
	pred_rewriting(P, P1), !.




init_aux_pred_name(Pred, N, AuxName, AuxN) :-
	g_read(aux, Aux),
	Aux1 is (Aux + 1) /\ (1 << 26 - 1),  % avoid negative numbers
	g_assign(aux, Aux1),
	'$make_aux_name'(Pred, N, Aux, AuxName),
	(   test_pred_info(bpl, Pred, N), % useful ?
	    set_pred_info(bpl, AuxName, AuxN)
	;   test_pred_info(bfd, Pred, N),
	    set_pred_info(bfd, AuxName, AuxN)
	;   true
	), !.




linearize(Body, AuxPred, Where, LAuxSrcCl) :-
	(   Body = (P ; Q) ->
	    linearize(Q, AuxPred, Where, LAuxSrcCl1),
	    linearize1(P, AuxPred, Where, LAuxSrcCl2),
	    append(LAuxSrcCl2, LAuxSrcCl1, LAuxSrcCl)
	;
	    linearize1(Body, AuxPred, Where, LAuxSrcCl)
	).

/* should no longer occurs since detected in normalize_cuts - to be removed
linearize1(fail, _, _, []) :-
	g_read(optim_fail, t), !.
*/
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
