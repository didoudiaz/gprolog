/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog to WAM compiler                                          */
/* File  : syn_sugar.pl                                                    */
/* Descr.: pass 1: syntactic sugar removing                                */
/* Author: Daniel Diaz                                                     */
/*                                                                         */
/* Copyright (C) 1999,2000 Daniel Diaz                                     */
/*                                                                         */
/* GNU Prolog is free software; you can redistribute it and/or modify it   */
/* under the terms of the GNU General Public License as published by the   */
/* Free Software Foundation; either version 2, or any later version.       */
/*                                                                         */
/* GNU Prolog is distributed in the hope that it will be useful, but       */
/* WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        */
/* General Public License for more details.                                */
/*                                                                         */
/* You should have received a copy of the GNU General Public License along */
/* with this program; if not, write to the Free Software Foundation, Inc.  */
/* 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     */
/*-------------------------------------------------------------------------*/

syntactic_sugar_init_pred(Pred,_):-
	'$aux_name'(Pred),
	!.

syntactic_sugar_init_pred(_,_):-
	g_assign(aux,1).




syntactic_sugar(SrcCl,Head,Body2):-
	(SrcCl=(Head:-Body) ; SrcCl=Head, Body=true),
        !,
	normalize_cuts(Body,Body1),
	normalize_alts(Body1,Head,Body2).



normalize_cuts(Body,Body2):-
	g_assign(has_cut,f),
        normalize_cuts1(Body,CutVar,Body1),
	!,
        (g_read(has_cut,t) 
             -> Body2=('$get_cut_level'(CutVar),Body1)
             ;  Body2=Body1).


normalize_cuts1(X,VarCut,P):-
	var(X),
	normalize_cuts1(call(X),VarCut,P).

normalize_cuts1((If;R),CutVar,Body):-
	nonvar(If),
	If=(P->Q),
	Body=('$get_cut_level'(CutVar1),P,'$cut'(CutVar1),Q1;R1),
	normalize_cuts1(Q,CutVar,Q1),
	normalize_cuts1(R,CutVar,R1).

normalize_cuts1((P->Q),CutVar,Body):-
	Body=('$get_cut_level'(CutVar1),P,'$cut'(CutVar1),Q1;fail),
	normalize_cuts1(Q,CutVar,Q1).

normalize_cuts1((!),CutVar,'$cut'(CutVar)):-
	g_assign(has_cut,t).

normalize_cuts1((P,Q),CutVar,(P1,Q1)):-
	normalize_cuts1(P,CutVar,P1),
	normalize_cuts1(Q,CutVar,Q1).

normalize_cuts1((P;Q),CutVar,(P1;Q1)):-
	normalize_cuts1(P,CutVar,P1),
	normalize_cuts1(Q,CutVar,Q1).

normalize_cuts1(call(G),_,'$call'(G,Func,Arity,true)):-
	cur_pred_without_aux(Func,Arity).

normalize_cuts1(catch(G,C,R),_,'$catch'(G,C,R,Func,Arity,true)):-
	cur_pred_without_aux(Func,Arity).

normalize_cuts1(throw(B),_,'$throw'(B,Func,Arity,true)):-
	cur_pred_without_aux(Func,Arity).

normalize_cuts1(P,_,P1):-
	(callable(P) -> true
                     ;  error('body goal is not callable (~q)',[P])),
	pred_rewriting(P,P1).




normalize_alts(Body,Head,Body1):-
	functor(Head,Pred,N),
	g_assign(head_functor,Pred),
	g_assign(head_arity,N),
	normalize_alts1(Body,Head,Body1),
	!.


normalize_alts1(X,_,call(X)):-
	var(X).

normalize_alts1((P,Q),RestC,(P1,Q1)):-
	normalize_alts1(P,(RestC,Q),P1),
	normalize_alts1(Q,(RestC,P),Q1).

normalize_alts1((P;Q),RestC,AuxPred):-
	lst_var(RestC,[],VarRestC),
	lst_var((P;Q),[],VarAlt),
	set_inter(VarAlt,VarRestC,V),
	length(V,N1),
	g_read(head_functor,Pred),
	g_read(head_arity,N),
	init_aux_pred_name(Pred,N,AuxName,N1),
	AuxPred=..[AuxName|V],
	g_read(where,Where),
	linearize((P;Q),AuxPred,Where,LAuxSrcCl),
	asserta(buff_aux_pred(AuxName,N1,LAuxSrcCl)).

normalize_alts1(P,_,P).




init_aux_pred_name(Pred,N,AuxName,N1):-
	g_read(aux,Aux),
	Aux1 is Aux+1,
	g_assign(aux,Aux1),
	'$make_aux_name'(Pred,N,Aux,AuxName),
	(test_pred_info(bpl,Pred,N), set_pred_info(bpl,AuxName,N1)
               ;
         test_pred_info(bfd,Pred,N), set_pred_info(bfd,AuxName,N1)
               ;
         true),
	!.




linearize(Body,AuxPred,Where,LAuxSrcCl):-
	(Body=(P;Q) -> linearize(Q,AuxPred,Where,LAuxSrcCl1),
	               linearize1(P,AuxPred,Where,LAuxSrcCl2),
		       append(LAuxSrcCl2,LAuxSrcCl1,LAuxSrcCl)
                    ;
                       linearize1(Body,AuxPred,Where,LAuxSrcCl)).


linearize1(fail,_,_,[]):-
	!.

linearize1(P,AuxPred,Where,[Where+AltP]):-
	copy_term((AuxPred:-P),AltP).




lst_var(X,V,V1):-
	var(X),
	!,
	set_add(V,X,V1).

lst_var(P,V,V1):-
	functor(P,_,N),
	lst_var_args(1,N,P,V,V1).



lst_var_args(I,N,P,V,V2):-
	(I=<N -> arg(I,P,ArgP),
                 lst_var(ArgP,V,V1),
		 I1 is I+1,
		 lst_var_args(I1,N,P,V1,V2)
              ;
                 V2=V).




        /* Other predicate rewriting */

pred_rewriting(fd_tell(X),'$call_c_test'(X)):-            % FD transformation
	test_call_c_allowed(fd_tell/1),
	!.

pred_rewriting(set_bip_name(Name,Arity),Pred1):-
	g_read(inline,t),             % also if -wbc since implies -no-inline
        nonvar(Name),
	nonvar(Arity),
	!,
	(atom(Name)
            ;
         error('set_bip_name/2 inline: atom expected instead of ~q',[Name])),
        (integer(Arity)
            ;
         error('set_bip_name/2 inline: integer expected instead of ~q',[Arity])),
	!,
	Pred1='$call_c'('Set_Bip_Name_2'(Name,Arity)).

pred_rewriting(Pred,Pred1):-                        % math define current bip
	g_read(inline,t),
	g_read(fast_math,f),
	functor(Pred,F,2),
	(F=(is) ; math_cmp_functor_name(F,_)),              % see code_gen.pl
	!,
	Pred1=('$call_c'('Set_Bip_Name_2'(F,2)),Pred).

pred_rewriting(P,P).




test_call_c_allowed(_):-
	g_read(call_c,t),
	!.

test_call_c_allowed(X):-
	error('~q not allowed in this mode',[X]).




add_wrapper_to_dyn_clause(Pred,N,Where+Cl,AuxName):-
	init_aux_pred_name(Pred,N,AuxName,N),
	(Cl=(Head:-Body)
             -> head_wrapper(Head,AuxName,Head1),
                Cl1=(Head1:-Body)
             ;
                head_wrapper(Cl,AuxName,Cl1)),
	assertz(buff_aux_pred(AuxName,N,[Where+Cl1])).


head_wrapper(Head,AuxName,Head1):-
	Head =..[_|LArgs],
	Head1=..[AuxName|LArgs].
