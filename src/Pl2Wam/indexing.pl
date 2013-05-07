/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog to WAM compiler                                          *
 * File  : indexing.pl                                                     *
 * Descr.: indexing code generation                                        *
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
 * Level 1:                                                                *
 *                                                                         *
 * The clauses C1,...,Cn of a predicate Pred are split into groups         *
 * G0,...,Gm so that each group Gi:                                        *
 *                                                                         *
 *   a) contains only one clause whose 1st arg is a variable.              *
 *   b) contains only clauses whose 1st arg is not a variable.             *
 *                                                                         *
 * The following code is then produced:                                    *
 *                                                                         *
 *   L0: try_me_else(L1)                                                   *
 *       <code for G0>                                                     *
 *                                                                         *
 *   L1: retry_me_else(L2)                                                 *
 *       <code for G1>                                                     *
 *            :                                                            *
 *            :                                                            *
 *   Lm: trust_me_else_fail                                                *
 *       <code for Gm>                                                     *
 *                                                                         *
 * Level 2:                                                                *
 *                                                                         *
 * For a group Gi whose type is a), the <code for Gi> only contains the    *
 * code produced for the associated Ck clause.                             *
 * For a group Gi whose type is b), the <code for Gi> contains indexing    *
 * instructions for the level 2 to discriminate between atoms, integers    *
 * lists and structures as follows:                                        *
 *                                                                         *
 *            switch_on_term(LabVar,LabAtm,LabInt,LabLst,LabStc)           *
 *                                                                         *
 *   LabFail: fail                 if there is a LabXxx = LabFail          *
 *                                                                         *
 *   LabAtm : switch_on_atom(N,[(atm1,LabAtm1),...(atmN,LabAtmN)])       \ *
 *                                                                       | *
 *   LabAtmj: try(Adj1)                  \  if more than 1 clause has    | *
 *            retry(Adj2) if more than 2 |  atmj as 1st arg,             | *
 *              :                        |  else LabAtmj = Adj1          | *
 *            trust(Adjk)                /                               | *
 *                                 if there are atms, else LabAtm=LabFail/ *
 *   idem for switch_on_integer                                            *
 *                                                                         *
 *   LabLst : try(Adj1)                  \  if more than 1 clause has    | *
 *            retry(Adj2) if more than 2 |  [_|_] as 1st arg,            | *
 *              :                        |  else LabLst = Adj1           | *
 *            trust(Adjk)                /                               | *
 *                                 if there are lsts, else LabLst=LabFail/ *
 *                                                                         *
 *   LabStc : switch_on_structure(N,[(stc1,LabStc1),...(stcN,LabStcN)])  \ *
 *                                                                       | *
 *   LabStcj: try(Adj1)                  \  if more than 1 clause has    | *
 *            retry(Adj2) if more than 2 |  stcj as 1st arg,             | *
 *              :                        |  else LabStcj = Adj1          | *
 *            trust(Adjk)                /                               | *
 *                                 if there are stcs, else LabStc=LabFail/ *
 *                                                                         *
 *   LabVar:  try_me_else(LabVar2) if there are more than 1 clause in Gi,  *
 *   Ad1:     <code for clause 1>  else LabVar = Ad1                       *
 *                                                                         *
 *   LabVar2: retry_me_else(LabVar3)                                       *
 *   Ad2:     <code for clause 2>                                          *
 *                :                                                        *
 *                :                                                        *
 *   LabVarp: trust_me_else_fail                                           *
 *   Adp:     <code for clause p>                                          *
 *                                                                         *
 * LCC: [cl(Ad,FirstArg,WamCl), ...] list of compiled clauses for Pred.    *
 *                                                                         *
 *       Ad      : will contain (in level 2) the label associated to WamCl *
 *                 (initially Ad is an unbound variable).                  *
 *       FirstArg: the first argument of the source clause.                *
 *       WamCl   : [wam_inst, ...] clause wam code.                        *
 *                                                                         *
 * look_for_var partitions LCC in LCCBefore, CCVar and LCCAfter and detects*
 * the current case:                                                       *
 *                                                                         *
 *   1...) a variables has been found (thus level 1), sub-cases:           *
 *    11) LCCBefore<>[] and LCCAfter<>[]  12) LCCBefore<>[] and LCCAfter=[]*
 *    13) LCCBefore= [] and LCCAfter<>[]  14) LCCBefore= [] and LCCAfter=[]*
 *                                                                         *
 *   2) no variables (thus level 2).                                       *
 *                                                                         *
 * other used variables:                                                   *
 *                                                                         *
 * Lev1: has any try/retry/trust_me_else been generated for level 1 (t/f)? *
 * Atm : [a(atm,[Ad, ...]), ...]                                           *
 * Int : [a(int,[Ad, ...]), ...]                                           *
 * Lst : [Ad, ...]                                                         *
 * Stc : [a(f/n,[Ad, ...]), ...]                                           *
 * List: Atm, Int, or Stc for general processings                          *
 *                                                                         *
 * Each label issued from the indexing phase is first referenced and later *
 * defined.                                                                *
 *-------------------------------------------------------------------------*/

indexing(LCC, WamCode1) :-
	indexing1(LCC, f, _, [_|WamCode]),       % ignore the unused label(0)
	cur_pred(Pred, N),
	(   test_pred_info(cut, Pred, N) ->
	    N1 is N + 1,
	    WamCode1 = [pragma_arity(N1), get_current_choice(x(N))|WamCode]
	;   WamCode1 = WamCode
	),
	allocate_labels(WamCode1, 1, _).




indexing1(LCC, Lev1, Lab, [label(Lab)|WamCode]) :-
	look_for_var(LCC, Case, LCCBefore, CCVar, LCCAfter),
	mk_indexing(Case, LCCBefore, CCVar, LCCAfter, Lev1, WamCode), !.




look_for_var([], 2, [], _, []).

look_for_var([cl(Ad, var, WamCl)|LCC], Case, [], cl(Ad, var, WamCl), LCC) :-
	!,
	(   LCC = [] ->
	    Case = 14
	;   Case = 13
	).

look_for_var([CC|LCC], Case1, [CC|LCCBefore], CCVar, LCCAfter) :-
	look_for_var(LCC, Case, LCCBefore, CCVar, LCCAfter),
	(   Case = 13 ->
	    Case1 = 11
	;   Case = 14 ->
	    Case1 = 12
	;   Case1 = Case
	).




mk_indexing(11, LCCBefore, cl(_, _, WamCl), LCCAfter, Lev1, WamCode) :-
	(   Lev1 = f ->
	    TmRmTm = try_me_else(Lab)
	;   TmRmTm = retry_me_else(Lab)
	),
	mk_indexing(2, LCCBefore, _, _, f, WamBefore),
	indexing1(LCCAfter, t, Lab1, WamAfter),
	WamCode = [TmRmTm, WamBefore, label(Lab), retry_me_else(Lab1), WamCl|WamAfter].

mk_indexing(12, LCCBefore, cl(_, _, WamCl), _, Lev1, WamCode) :-
	(   Lev1 = f ->
	    TmRmTm = try_me_else(Lab)
	;   TmRmTm = retry_me_else(Lab)
	),
	mk_indexing(2, LCCBefore, _, _, f, WamBefore),
	WamCode = [TmRmTm, WamBefore, label(Lab), trust_me_else_fail|WamCl].

mk_indexing(13, _, cl(_, _, WamCl), LCCAfter, Lev1, WamCode) :-
	(   Lev1 = f ->
	    TmRmTm = try_me_else(Lab)
	;   TmRmTm = retry_me_else(Lab)
	),
	indexing1(LCCAfter, t, Lab, WamAfter),
	WamCode = [TmRmTm, WamCl|WamAfter].

mk_indexing(14, _, cl(_, _, WamCl), _, Lev1, WamCode) :-
	(   Lev1 = f ->
	    WamCode = WamCl
	;   WamCode = [trust_me_else_fail|WamCl]
	).

mk_indexing(2, LCC, _, _, Lev1, WamCode) :-
	(   Lev1 = f ->
	    WamCode = WamCode1
	;   WamCode = [trust_me_else_fail|WamCode1]
	),
	(   LCC = [_] ->              % no switch_on_term for only one clause
	    WamCode2 = [_|WamCode2Rest],               % remove useless label
	    WamCode1 = WamCode2Rest
	;   WamCode1 = [switch_on_term(LabVar, LabAtm, LabInt, LabLst, LabStc)|WamCode2]
	),
	WamCode2 = WamSwtAtm,
	split(LCC, Atm, Int, Lst, Stc), !,
	gen_switch(Atm, switch_on_atom, LabAtm, WamSwtInt, WamSwtAtm),
	gen_switch(Int, switch_on_integer, LabInt, WamLst, WamSwtInt),
	gen_list(Lst, LabLst, WamSwtStc, WamLst),
	gen_switch(Stc, switch_on_structure, LabStc, WamCode3, WamSwtStc),
	gen_insts(LCC, LabVar, WamCode3).




split(LCC, Atm1, Int1, Lst, Stc1) :-
	split1(LCC, [], [], a(End, End), [], Atm, Int, a([], Lst), Stc),
	terminate_list(Atm, Atm1),
	terminate_list(Int, Int1),
	terminate_list(Stc, Stc1).


split1([], Atm, Int, Lst, Stc, Atm, Int, Lst, Stc).

split1([cl(Ad, FirstArg, _)|LCC], Atm, Int, Lst, Stc, Atm2, Int2, Lst2, Stc2) :-
	split2(FirstArg, Ad, Atm, Int, Lst, Stc, Atm1, Int1, Lst1, Stc1),
	split1(LCC, Atm1, Int1, Lst1, Stc1, Atm2, Int2, Lst2, Stc2).


split2(atm(A), Ad, Atm, Int, Lst, Stc, Atm1, Int, Lst, Stc) :-
	add_to_list(Atm, A, Ad, Atm1).

split2(int(N), Ad, Atm, Int, Lst, Stc, Atm, Int1, Lst, Stc) :-
	add_to_list(Int, N, Ad, Int1).

split2(lst, Ad, Atm, Int, a([Ad|End], LAd), Stc, Atm, Int, a(End, LAd), Stc).

split2(stc(F, N), Ad, Atm, Int, Lst, Stc, Atm, Int, Lst, Stc1) :-
	add_to_list(Stc, F / N, Ad, Stc1).




add_to_list([], F, Ad, [a(F, End, [Ad|End])]).

add_to_list([a(F, [Ad|End], LAd)|List], F, Ad, [a(F, End, LAd)|List]) :-
	!.

add_to_list([X|List], F, Ad, [X|List1]) :-
	add_to_list(List, F, Ad, List1).




terminate_list([], []).

terminate_list([a(F, [], LAd)|L], [a(F, LAd)|L1]) :-
	terminate_list(L, L1).




gen_switch([], _, fail, LNext, LNext) :-
	!.

    % if only 1 element with only 1 clause, no switch (remove if needed)

gen_switch([a(_, [Ad])], _, Ad, LNext, LNext) :-
	!.

    % if only 1 element with n clauses, no switch (remove if needed)
/*
gen_switch([a(_, LAd)], _, Lab, LNext, WamTRT) :-
	!,
	gen_list(LAd, Lab, LNext, WamTRT).
*/
gen_switch(List, Ins, Lab, LNext, [label(Lab), SwtW|WamTRT]) :-
	create_switch_list(List, LSwt, LNext, WamTRT),
	SwtW =.. [Ins, LSwt].




create_switch_list([], [], LNext, LNext).

create_switch_list([a(F, LAd)|List], [(F, Lab)|LSwt], LNext, WamTRT) :-
	gen_list(LAd, Lab, WamTRT1, WamTRT),
	create_switch_list(List, LSwt, LNext, WamTRT1).




gen_list([], fail, LNext, LNext).

gen_list([Ad], Ad, LNext, LNext) :-                % only 1 Atmj, Lst or Stcj
	!.

gen_list([Ad|LAd], Lab, LNext, WamRT1) :-                              % 2..n
	WamRT1 = [label(Lab), try(Ad)|WamRT],
	gen_list1(LAd, LNext, WamRT).


gen_list1([Ad], LNext, [trust(Ad)|LNext]).

gen_list1([Ad|LAd], LNext, WamRT1) :-
	WamRT1 = [retry(Ad)|WamRT],
	gen_list1(LAd, LNext, WamRT).




gen_insts([cl(Ad, _, WamCl)], Ad, [label(Ad)|WamCl]) :-       % only 1 clause
	!.

gen_insts([cl(Ad, _, WamCl)|LCC], Lab, WamCode2) :-                    % 2..n
	gen_insts1(LCC, Lab1, WamCode),
	WamCode2 = [label(Lab), try_me_else(Lab1), label(Ad), WamCl|WamCode].



gen_insts1([cl(Ad, _, WamCl)], Lab, [label(Lab), trust_me_else_fail, label(Ad)|WamCl]) :-
	!.

gen_insts1([cl(Ad, _, WamCl)|LCC], Lab, WamCode2) :-
	gen_insts1(LCC, Lab1, WamCode),
	WamCode2 = [label(Lab), retry_me_else(Lab1), label(Ad), WamCl|WamCode].




allocate_labels([], N, N) :-
	!.

allocate_labels([WamInst1|WamInst2], N, N2) :-
	!,
	allocate_labels(WamInst1, N, N1),                  % for nested lists
	allocate_labels(WamInst2, N1, N2).

allocate_labels(label(N), N, N1) :-
	!,
	N1 is N + 1 .

allocate_labels(_, N, N).
