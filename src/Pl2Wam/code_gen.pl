/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog to WAM compiler                                          *
 * File  : code_gen.pl                                                     *
 * Descr.: pass 3: code generation                                         *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2003 Daniel Diaz                                     *
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

code_generation(Head, Body, NbChunk, NbY, WamHead) :-
	g_assign(last_pred, f),
	generate_head(Head, NbChunk, NbY, WamBody, WamHead),
	generate_body(Body, NbChunk, WamBody).




generate_head(p(_, _ / N, LArg), NbChunk, NbY, WamNext, WamHead) :-
	gen_list_integers(0, N, LReg),
	(   g_read(reorder, t) ->
	    reorder_head_arg_lst(LArg, LReg, LArg1, LReg1)
	;   LArg1 = LArg,
	    LReg1 = LReg
	),
	gen_unif_arg_lst(LArg1, LReg1, WamNext, WamLArg),
	(   NbChunk > 1 ->
	    WamHead = [allocate(NbY)|WamLArg]
	;   WamHead = WamLArg
	).




reorder_head_arg_lst(LArg, LReg, LArg1, LReg1) :-
	split_arg_lst(LArg, LReg, LArgK, LRegK, LArgS, LRegS, LArgT, LRegT),
	reverse(LArgT, LArgT1),
	reverse(LRegT, LRegT1),
	append(LArgK, LArgT1, LArgKT),
	append(LArgKT, LArgS, LArg1),
	append(LRegK, LRegT1, LRegKT),
	append(LRegKT, LRegS, LReg1).




generate_body([], _, [proceed]).

generate_body([p(NoPred, Pred / N, LArg)|Body], NbChunk, WamPred) :-
	(   NoPred = NbChunk ->
	    g_assign(last_pred, t)
	;   true
	),
	generate_body1(Pred, N, LArg, NoPred, Body, NbChunk, WamPred).


generate_body1(fail, 0, _, _, _, _, [fail]) :-
	!.

generate_body1('$call_c', 2, [Arg, LCOpt], NoPred, Body, NbChunk, WamArgs) :-
	!,
	(   Arg = atm(Name),
	    LStcArg = []
	;   Arg = stc(Name, _, LStcArg)
	),
	(   Body \== [], memberchk(jump, LCOpt) ->
	    LCOpt1 = [set_cp|LCOpt]
	;   LCOpt1 = LCOpt
        ),
	load_c_call_args(LCOpt, LStcArg, LValue, WamCallC1, WamArgs),
	WamCallCInst = call_c(Name, LCOpt1, LValue),
	(   Body = [] ->
	    (   NoPred > 1 ->
	        WamCallC1 = [deallocate, WamCallCInst, proceed]
	    ;   WamCallC1 = [WamCallCInst, proceed]
	    )
	;
	    WamCallC1 = [WamCallCInst|WamBody],
	    generate_body(Body, NbChunk, WamBody)
	).

generate_body1(Pred, N, LArg, NoPred, Body, NbChunk, WamPred) :-
	inline_predicate(Pred, N),
	!,
	gen_inline_pred(Pred, N, LArg, WamBody, WamPred), !,
	(   Body = [] ->
	    (   NoPred > 1 ->
	        WamBody = [deallocate, proceed]
	    ;   WamBody = [proceed]
	    )
	;   generate_body(Body, NbChunk, WamBody)
	).

generate_body1(Pred, N, LArg, NoPred, Body, NbChunk, WamLArg) :-
	gen_list_integers(0, N, LReg),
	(   g_read(reorder, t) ->
	    reorder_body_arg_lst(LArg, LReg, LArg1, LReg1)
	;   LArg1 = LArg,
	    LReg1 = LReg
	),
	gen_load_arg_lst(LArg1, LReg1, WamCallExecute, WamLArg),
	(   Body = [] ->
	    (   NoPred > 1 ->
	        WamCallExecute = [deallocate, execute(Pred / N)]
	    ;   WamCallExecute = [execute(Pred / N)]
	    )
	;   WamCallExecute = [call(Pred / N)|WamBody],
	    generate_body(Body, NbChunk, WamBody)
	).




reorder_body_arg_lst(LArg, LReg, LArg1, LReg1) :-
	split_arg_lst(LArg, LReg, LArgK, LRegK, LArgS, LRegS, LArgT, LRegT),
	append(LArgS, LArgT, LArgST),
	append(LArgST, LArgK, LArg1),
	append(LRegS, LRegT, LRegST),
	append(LRegST, LRegK, LReg1).




          % split LArg/LReg in:
          %       LArgK/LRegK: known elements (without temporaries)
          %       LArgS/LRegS: structures containing temporaries
          %       LArgT/LRegT: temporaries

split_arg_lst([], [], [], [], [], [], [], []).

split_arg_lst([Arg|LArg], [Reg|LReg], LArgK, LRegK, LArgS, LRegS, LArgT, LRegT) :-
	(   Arg = var(x(No), _),
	    No \== void,
	    LArgK = LArgK1,
	    LRegK = LRegK1,
	    LArgS = LArgS1,
	    LRegS = LRegS1,
	    LArgT = [Arg|LArgT1],
	    LRegT = [Reg|LRegT1]
	;   Arg = stc(_, _, LStcArg),
	    has_temporaries(LStcArg),
	    LArgK = LArgK1,
	    LRegK = LRegK1,
	    LArgS = [Arg|LArgS1],
	    LRegS = [Reg|LRegS1],
	    LArgT = LArgT1,
	    LRegT = LRegT1
	;   LArgK = [Arg|LArgK1],
	    LRegK = [Reg|LRegK1],
	    LArgS = LArgS1,
	    LRegS = LRegS1,
	    LArgT = LArgT1,
	    LRegT = LRegT1
	), !,
	split_arg_lst(LArg, LReg, LArgK1, LRegK1, LArgS1, LRegS1, LArgT1, LRegT1).




has_temporaries([Arg|LArg]) :-
	(   Arg = var(x(No), _),
	    No \== void
	;   Arg = stc(_, _, LStcArg),
	    has_temporaries(LStcArg)
	;   has_temporaries(LArg)
	), !.




	% gen_unif_arg_lst(LArg, LReg, WamNext, WamLArg)

gen_unif_arg_lst([], [], WamNext, WamNext).

gen_unif_arg_lst([Arg|LArg], [Reg|LReg], WamNext, WamArg) :-
	gen_unif_arg(Arg, Reg, WamLArg, WamArg),
	gen_unif_arg_lst(LArg, LReg, WamNext, WamLArg).




	% gen_unif_arg(Arg, Reg, WamNext, WamArg)

gen_unif_arg(var(VarName, Info), Reg, WamNext, WamArg) :-
	(   var(Info) ->
	    (   VarName == x(void) ->
	        WamArg = WamNext
	    ;   Info = not_in_cur_env,
	        WamArg = [get_variable(VarName, Reg)|WamNext]
	    )
	;   WamArg = [get_value(VarName, Reg)|WamNext]
	).

gen_unif_arg(atm(A), Reg, WamNext, [get_atom(A, Reg)|WamNext]).

gen_unif_arg(int(N), Reg, WamNext, [get_integer(N, Reg)|WamNext]).

gen_unif_arg(flt(N), Reg, WamNext, [get_float(N, Reg)|WamNext]).

gen_unif_arg(nil, Reg, WamNext, [get_nil(Reg)|WamNext]).

gen_unif_arg(stc(F, N, LStcArg), Reg, WamNext, [WamInst|WamStcArg]) :-
	(   F = '.',
	    N = 2 ->
	    WamInst = get_list(Reg)
	;   WamInst = get_structure(F / N, Reg)
	),
	flat_stc_arg_lst(LStcArg, head, LStcArg1, LArgAux, LRegAux),
	gen_subterm_arg_lst(LStcArg1, WamArgAux, WamStcArg),
	gen_unif_arg_lst(LArgAux, LRegAux, WamNext, WamArgAux).




	% gen_load_arg_lst(LArg, LReg, WamNext, WamLArg)

gen_load_arg_lst([], [], WamNext, WamNext).

gen_load_arg_lst([Arg|LArg], [Reg|LReg], WamNext, WamArg) :-
	gen_load_arg(Arg, Reg, WamLArg, WamArg),
	gen_load_arg_lst(LArg, LReg, WamNext, WamLArg).




	% gen_load_arg(Arg, Reg, WamNext, WamArg)

gen_load_arg(var(VarName, Info), Reg, WamNext, [WamInst|WamNext]) :-
	(   var(Info) ->
	    (   VarName == x(void) ->
	        WamInst = put_void(Reg)
	    ;   (   VarName = x(_) ->
	            Info = in_heap
	        ;   Info = unsafe
	        ),
	        WamInst = put_variable(VarName, Reg)
	    )
	;   Info = unsafe,
	    g_read(last_pred, t) ->
	    WamInst = put_unsafe_value(VarName, Reg)
	;   WamInst = put_value(VarName, Reg)
	).

gen_load_arg(atm(A), Reg, WamNext, [put_atom(A, Reg)|WamNext]).

gen_load_arg(int(N), Reg, WamNext, [put_integer(N, Reg)|WamNext]).

gen_load_arg(flt(N), Reg, WamNext, [put_float(N, Reg)|WamNext]).

gen_load_arg(nil, Reg, WamNext, [put_nil(Reg)|WamNext]).

gen_load_arg(stc(F, N, LStcArg), Reg, WamNext, WamArgAux) :-
	(   F = '.',
	    N = 2 ->
	    WamInst = put_list(Reg)
	;   WamInst = put_structure(F / N, Reg)
	),
	flat_stc_arg_lst(LStcArg, body, LStcArg1, LArgAux, LRegAux),
	gen_load_arg_lst(LArgAux, LRegAux, [WamInst|WamStcArg], WamArgAux),
	gen_subterm_arg_lst(LStcArg1, WamNext, WamStcArg).




          % flat_stc_arg_lst(LStcArg, HB, LStcArg1, LArgAux, LRegAux)

flat_stc_arg_lst([], _, [], [], []).

flat_stc_arg_lst([StcArg|LStcArg], HB, [StcArg|LStcArg1], LArgAux, LRegAux) :-
	simple_stc_arg(StcArg), !,
	flat_stc_arg_lst(LStcArg, HB, LStcArg1, LArgAux, LRegAux).

flat_stc_arg_lst([StcArg], HB, [stc(F, N, LStcArg1)], LArgAux, LRegAux) :-
	g_read(opt_last_subterm, t),     % last subterm unif stc optimization
	StcArg = stc(F, N, LStcArg), !,
	flat_stc_arg_lst(LStcArg, HB, LStcArg1, LArgAux, LRegAux).

flat_stc_arg_lst([StcArg|LStcArg], HB, [V|LStcArg1], [StcArg|LArgAux], [X|LRegAux]) :-
	(   HB = head ->
	    V = var(x(X), _)
	;   V = var(x(X), in_heap)
	),
	flat_stc_arg_lst(LStcArg, HB, LStcArg1, LArgAux, LRegAux).



simple_stc_arg(var(_, _)).

simple_stc_arg(atm(_)).

simple_stc_arg(int(_)).

simple_stc_arg(nil).




           % gen_subterm_arg_lst(LStcArg, WamNext, WamLStcArg)

gen_subterm_arg_lst([], WamNext, WamNext).

gen_subterm_arg_lst([Arg|LArg], WamNext, WamArg) :-
	gen_compte_void([Arg|LArg], 0, N, LArg1),
	(   N = 0 ->
	    gen_subterm_arg(Arg, WamLArg, WamArg),
	    gen_subterm_arg_lst(LArg, WamNext, WamLArg)
	;   WamArg = [unify_void(N)|WamLArg1],
	    gen_subterm_arg_lst(LArg1, WamNext, WamLArg1)
	).




gen_compte_void([var(x(No), _)|LArg], N, N2, LArg1) :-
	No == void, !,
	N1 is N + 1,
	gen_compte_void(LArg, N1, N2, LArg1).

gen_compte_void(LArg, N, N, LArg).




gen_subterm_arg(var(VarName, Info), WamNext, [WamInst|WamNext]) :-
	(   var(Info) ->
	    Info = in_heap,
	    WamInst = unify_variable(VarName)
	;   Info = in_heap ->
	    WamInst = unify_value(VarName)
	;   WamInst = unify_local_value(VarName)
	).

gen_subterm_arg(atm(A), WamNext, [unify_atom(A)|WamNext]).

gen_subterm_arg(int(N), WamNext, [unify_integer(N)|WamNext]).

gen_subterm_arg(nil, WamNext, [unify_nil|WamNext]).

gen_subterm_arg(stc(F, N, LStcArg), WamNext, [WamInst|WamLStcArg]) :-
	(   F = '.',
	    N = 2 ->
	    WamInst = unify_list
	;   WamInst = unify_structure(F / N)
	),
	gen_subterm_arg_lst(LStcArg, WamNext, WamLStcArg).




gen_list_integers(I, N, L) :-
	(   I < N ->
	    L = [I|L1],
	    I1 is I + 1,
	    gen_list_integers(I1, N, L1)
	;   L = []
	).




          % called at code emission

special_form(put_variable(x(X), X), put_void(X)).




dummy_instruction(get_variable(x(X), X), f).
dummy_instruction(put_value(x(X), X), f).




	% Inline predicate code generation:
	%      gen_inline_pred(Pred, Arity, LArg, WamNext, WamPred)
        %
	% the predicates defined here must have a corresponding clause
	% inline_predicate/2 (in pass 2).

:-	discontiguous(gen_inline_pred / 5).


	% Cut inline ('$get_cut_level'/1,'$cut'/1)

gen_inline_pred('$get_cut_level', 1, [Arg], WamNext, WamArg) :-
	cur_pred(Pred, N),
	set_pred_info(cut, Pred, N),
	gen_unif_arg(Arg, N, WamNext, WamArg).

gen_inline_pred('$cut', 1, [var(VarName, _)], WamNext, [WamInst|WamNext]) :-
	WamInst = cut(VarName).




	% Unification inline (=/2)

gen_inline_pred(=, 2, [Arg1, Arg2], WamNext, WamEqual) :-
	equal(Arg1, Arg2, WamNext, WamEqual), !.




equal(Arg1, Arg2, WamNext, WamNext) :-
	Arg1 == Arg2.

equal(var(VarName, Info), var(VarName, Info), WamNext, WamNext) :-
	var(Info).

equal(var(VarName1, Info1), Arg2, WamNext, WamEqual) :-
	(   VarName1 = x(Reg1) ->
	    (   Reg1 == void ->
	        WamNext = WamEqual
	    ;   inline_unif_reg_term(Info1, Reg1, Arg2, WamNext, WamEqual)
	    )
	;   gen_load_arg(var(VarName1, Info1), IReg, WamEqual1, WamEqual),
	    gen_unif_arg(Arg2, IReg, WamNext, WamEqual1)
	).

equal(Arg1, var(VarName2, Info2), WamNext, WamEqual) :-
	(   VarName2 = x(Reg2) ->
	    (   Reg2 == void ->
	        WamNext = WamEqual
	    ;   inline_unif_reg_term(Info2, Reg2, Arg1, WamNext, WamEqual)
	    )
	;   gen_load_arg(var(VarName2, Info2), IReg, WamEqual1, WamEqual),
	    gen_unif_arg(Arg1, IReg, WamNext, WamEqual1)
	).

equal(Arg1, var(x(Reg2), Info2), WamNext, WamEqual) :-
	inline_unif_reg_term(Info2, Reg2, Arg1, WamNext, WamEqual).

equal(stc(F, N, LStcArg1), stc(F, N, LStcArg2), WamNext, WamEqual) :-
	equal_lst(LStcArg1, LStcArg2, WamNext, WamEqual).

equal(_, _, WamNext, [fail|WamNext]) :-
	warn('explicit unification will fail', []).




equal_lst([], [], WamNext, WamNext).

equal_lst([Arg1|LArg1], [Arg2|LArg2], WamNext, WamEqual) :-
	equal(Arg1, Arg2, WamLArg, WamEqual),
	equal_lst(LArg1, LArg2, WamNext, WamLArg).




inline_unif_reg_term(Info, Reg, Arg, WamNext, WamUnif) :-
	(   var(Info) ->
	    Info = in_heap,
	    gen_load_arg(Arg, Reg, WamNext, WamUnif)
	;   gen_unif_arg(Arg, Reg, WamNext, WamUnif)
	).




	% Mathematical inlines (is/2, =:=/2, ...)
/* provisional... pb with allocator to reuse VN2 for VN1
gen_inline_pred(is, 2, [var(VN1, Info1), stc(+, 2, [var(VN2, Info2), int(1)])], WamNext, WamMath) :-
	var(Info1),
	!,
	(   var(Info2) ->
	    error('unbound variable in arithmetic expression', [])
	;   true
	),
	Info1 = not_in_cur_env,
	WamMath = [call_c('Math_X_is_inc_y',[fast],[&,VN1,VN2])|WamNext].
*/
gen_inline_pred(is, 2, [Arg1, Arg2], WamNext, WamMath) :-
	load_math_expr(Arg2, Reg, WamUnif, WamMath), !,
	gen_unif_arg(Arg1, Reg, WamNext, WamUnif).



load_math_expr(var(VarName, Info), Reg, WamNext, WamMath) :-
	(   var(Info) ->
	    error('unbound variable in arithmetic expression', [])
	;   true
	),
	(   g_read(fast_math, t) ->
	    WamMath = [math_fast_load_value(VarName, Reg)|WamNext]
	;   WamMath = [math_load_value(VarName, Reg)|WamNext]
	).

load_math_expr(int(N), Reg, WamNext, WamMath) :-
	gen_load_arg(int(N), Reg, WamNext, WamMath).

load_math_expr(flt(N), Reg, WamNext, WamMath) :-
	gen_load_arg(flt(N), Reg, WamNext, WamMath).

load_math_expr(stc(F, N, LArg), Reg, WamNext, WamMath) :-
	load_math_expr1(F, N, LArg, Reg, WamNext, WamMath).

load_math_expr(X, _, _, _) :-
	error('unknown expression in arithmetic expression (~q)', [X]).


load_math_expr1('.', 2, [Arg, nil], Reg, WamNext, WamMath) :-
	load_math_expr(Arg, Reg, WamNext, WamMath).

load_math_expr1(+, 1, [Arg], Reg, WamNext, WamMath) :-
	load_math_expr(Arg, Reg, WamNext, WamMath).

load_math_expr1(+, 2, [Arg1, int(1)], Reg, WamNext, WamMath) :-
	load_math_expr1(inc, 1, [Arg1], Reg, WamNext, WamMath).

load_math_expr1(-, 2, [Arg1, int(1)], Reg, WamNext, WamMath) :-
	load_math_expr1(dec, 1, [Arg1], Reg, WamNext, WamMath).

load_math_expr1(F, N, LArg, Reg, WamNext, WamMath) :-
	(   g_read(fast_math, t) ->
	    fast_exp_functor_name(F, N, Name)
	;   math_exp_functor_name(F, N, Name)
	),
	load_math_arg_lst(LArg, LValue, WamInst, WamMath),
	WamInst = [call_c(Name, [fast_call,x(Reg)], LValue)|WamNext].

load_math_expr1(F, N, _, _, _, _) :-
	math_exp_functor_name(F, N, _),
	error('arithmetic operation not allowed in fast math (~q)', [F / N]).

load_math_expr1(F, N, _, _, _, _) :-
	error('unknown operation in arithmetic expression (~q)', [F / N]).




load_math_arg_lst([], [], WamNext, WamNext).

load_math_arg_lst([Arg|LArg], [x(Reg)|LReg], WamNext, WamMath) :-
	load_math_expr(Arg, Reg, WamLArg, WamMath),
	load_math_arg_lst(LArg, LReg, WamNext, WamLArg).




fast_exp_functor_name(-, 1, 'Fct_Fast_Neg').
fast_exp_functor_name(inc, 1, 'Fct_Fast_Inc').
fast_exp_functor_name(dec, 1, 'Fct_Fast_Dec').
fast_exp_functor_name(+, 2, 'Fct_Fast_Add').
fast_exp_functor_name(-, 2, 'Fct_Fast_Sub').
fast_exp_functor_name(*, 2, 'Fct_Fast_Mul').
fast_exp_functor_name(//, 2, 'Fct_Fast_Div').
fast_exp_functor_name(rem, 2, 'Fct_Fast_Rem').
fast_exp_functor_name(mod, 2, 'Fct_Fast_Mod').
fast_exp_functor_name(/\, 2, 'Fct_Fast_And').
fast_exp_functor_name(\/, 2, 'Fct_Fast_Or').
fast_exp_functor_name(^, 2, 'Fct_Fast_Xor').
fast_exp_functor_name(\, 1, 'Fct_Fast_Not').
fast_exp_functor_name(<<, 2, 'Fct_Fast_Shl').
fast_exp_functor_name(>>, 2, 'Fct_Fast_Shr').
fast_exp_functor_name(abs, 1, 'Fct_Fast_Abs').
fast_exp_functor_name(sign, 1, 'Fct_Fast_Sign').




math_exp_functor_name(-, 1, 'Fct_Neg').
math_exp_functor_name(inc, 1, 'Fct_Inc').
math_exp_functor_name(dec, 1, 'Fct_Dec').
math_exp_functor_name(+, 2, 'Fct_Add').
math_exp_functor_name(-, 2, 'Fct_Sub').
math_exp_functor_name(*, 2, 'Fct_Mul').
math_exp_functor_name(//, 2, 'Fct_Div').
math_exp_functor_name(/, 2, 'Fct_Float_Div').
math_exp_functor_name(rem, 2, 'Fct_Rem').
math_exp_functor_name(mod, 2, 'Fct_Mod').
math_exp_functor_name(/\, 2, 'Fct_And').
math_exp_functor_name(\/, 2, 'Fct_Or').
math_exp_functor_name(^, 2, 'Fct_Xor').
math_exp_functor_name(\, 1, 'Fct_Not').
math_exp_functor_name(<<, 2, 'Fct_Shl').
math_exp_functor_name(>>, 2, 'Fct_Shr').
math_exp_functor_name(abs, 1, 'Fct_Abs').
math_exp_functor_name(sign, 1, 'Fct_Sign').

math_exp_functor_name(min, 2, 'Fct_Min').
math_exp_functor_name(max, 2, 'Fct_Max').
math_exp_functor_name(**, 2, 'Fct_Pow').
math_exp_functor_name(sqrt, 1, 'Fct_Sqrt').
math_exp_functor_name(atan, 1, 'Fct_Atan').
math_exp_functor_name(cos, 1, 'Fct_Cos').
math_exp_functor_name(acos, 1, 'Fct_Acos').
math_exp_functor_name(sin, 1, 'Fct_Sin').
math_exp_functor_name(asin, 1, 'Fct_Asin').
math_exp_functor_name(exp, 1, 'Fct_Exp').
math_exp_functor_name(log, 1, 'Fct_Log').
math_exp_functor_name(float, 1, 'Fct_Float').
math_exp_functor_name(ceiling, 1, 'Fct_Ceiling').
math_exp_functor_name(floor, 1, 'Fct_Floor').
math_exp_functor_name(round, 1, 'Fct_Round').
math_exp_functor_name(truncate, 1, 'Fct_Truncate').
math_exp_functor_name(float_fractional_part, 1, 'Fct_Float_Fract_Part').
math_exp_functor_name(float_integer_part, 1, 'Fct_Float_Integ_Part').




gen_inline_pred(F, 2, LArg, WamNext, WamMath) :-
	(   g_read(fast_math, t) ->
	    fast_cmp_functor_name(F, Name)
	;   math_cmp_functor_name(F, Name)
	),
	load_math_arg_lst(LArg, LValue, WamInst, WamMath),
	WamInst = [call_c(Name, [fast_call, boolean], LValue)|WamNext].



fast_cmp_functor_name(=:=, 'Blt_Fast_Eq').
fast_cmp_functor_name(=\=, 'Blt_Fast_Neq').
fast_cmp_functor_name(<, 'Blt_Fast_Lt').
fast_cmp_functor_name(=<, 'Blt_Fast_Lte').
fast_cmp_functor_name(>, 'Blt_Fast_Gt').
fast_cmp_functor_name(>=, 'Blt_Fast_Gte').

math_cmp_functor_name(=:=, 'Blt_Eq').
math_cmp_functor_name(=\=, 'Blt_Neq').
math_cmp_functor_name(<, 'Blt_Lt').
math_cmp_functor_name(=<, 'Blt_Lte').
math_cmp_functor_name(>, 'Blt_Gt').
math_cmp_functor_name(>=, 'Blt_Gte').




	% foreign C call

gen_inline_pred('$foreign_call_c', 1, [args(FctName, Return, BipPred, ChcSize, LType)], WamNext, WamInst) :-
	WamInst = [foreign_call_c(FctName, Return, BipPred, ChcSize, LType)|WamNext].




          % call_c/3 management predicates


load_c_call_args(LCOpt, LArg, LValue, WamNext, WamArg) :-
	memberchk(by_value, LCOpt),
	load_by_value_arg_lst(LArg, LValue, WamNext, WamArg), !.


load_c_call_args(_, LArg, LValue, WamNext, WamArg) :-
	load_by_reg_arg_lst(LArg, LValue, WamNext, WamArg), !.




load_by_reg_arg_lst([], [], WamNext, WamNext).

load_by_reg_arg_lst([Arg|LArg], [x(Reg)|LReg], WamNext, WamArg) :-
	gen_load_arg(Arg, Reg, WamLArg, WamArg),
	load_by_reg_arg_lst(LArg, LReg, WamNext, WamLArg).




load_by_value_arg_lst([], [], WamNext, WamNext).

load_by_value_arg_lst([Arg|LArg], [Value|LValue], WamNext, WamArg) :-
	load_by_value_arg(Arg, Value, WamLArg, WamArg),
	load_by_value_arg_lst(LArg, LValue, WamNext, WamLArg).


load_by_value_arg(atm(A), A, WamNext, WamNext).

load_by_value_arg(int(N), N, WamNext, WamNext).

load_by_value_arg(flt(N), N, WamNext, WamNext).

load_by_value_arg(nil, [], WamNext, WamNext).

load_by_value_arg(stc('/', 2, [atm(F), int(N)]), F/N, WamNext, WamNext).

load_by_value_arg(Arg, x(Reg), WamArg, WamNext) :-
	gen_load_arg(Arg, Reg, WamArg, WamNext).




          % Other inlines

gen_inline_pred(F, N, LArg, WamNext, WamCallC) :-
	c_fct_name(F, N, Name, RetType),
	(   RetType = bool ->
	    LCOpt = [fast_call, boolean]
	;   LCOpt = [fast_call]
	),
	load_c_call_args(LCOpt, LArg, LValue, WamInst, WamCallC),
	WamInst = [call_c(Name, LCOpt, LValue)|WamNext].



c_fct_name(var, 1, 'Blt_Var', bool).
c_fct_name(nonvar, 1, 'Blt_Non_Var', bool).
c_fct_name(atom, 1, 'Blt_Atom', bool).
c_fct_name(integer, 1, 'Blt_Integer', bool).
c_fct_name(float, 1, 'Blt_Float', bool).
c_fct_name(number, 1, 'Blt_Number', bool).
c_fct_name(atomic, 1, 'Blt_Atomic', bool).
c_fct_name(compound, 1, 'Blt_Compound', bool).
c_fct_name(callable, 1, 'Blt_Callable', bool).
c_fct_name(list, 1, 'Blt_List', bool).
c_fct_name(partial_list, 1, 'Blt_Partial_List', bool).
c_fct_name(list_or_partial_list, 1, 'Blt_List_Or_Partial_List', bool).

c_fct_name(fd_var, 1, 'Blt_Fd_Var', bool).
c_fct_name(non_fd_var, 1, 'Blt_Non_Fd_Var', bool).
c_fct_name(generic_var, 1, 'Blt_Generic_Var', bool).
c_fct_name(non_generic_var, 1, 'Blt_Non_Generic_Var', bool).


c_fct_name(arg, 3, 'Blt_Arg', bool).
c_fct_name(functor, 3, 'Blt_Functor', bool).
c_fct_name(compare, 3, 'Blt_Compare', bool).
c_fct_name(=.., 2, 'Blt_Univ', bool).

c_fct_name(==, 2, 'Blt_Term_Eq', bool).
c_fct_name(\==, 2, 'Blt_Term_Neq', bool).
c_fct_name(@<, 2, 'Blt_Term_Lt', bool).
c_fct_name(@=<, 2, 'Blt_Term_Lte', bool).
c_fct_name(@>, 2, 'Blt_Term_Gt', bool).
c_fct_name(@>=, 2, 'Blt_Term_Gte', bool).

c_fct_name(g_assign, 2, 'Blt_G_Assign', void).
c_fct_name(g_assignb, 2, 'Blt_G_Assignb', void).
c_fct_name(g_link, 2, 'Blt_G_Link', void).
c_fct_name(g_read, 2, 'Blt_G_Read', bool).
c_fct_name(g_array_size, 2, 'Blt_G_Array_Size', bool).
c_fct_name(g_inc, 1, 'Blt_G_Inc', void).
c_fct_name(g_inco, 2, 'Blt_G_Inco', bool).
c_fct_name(g_inc, 2, 'Blt_G_Inc_2', bool).
c_fct_name(g_inc, 3, 'Blt_G_Inc_3', bool).
c_fct_name(g_dec, 1, 'Blt_G_Dec', void).
c_fct_name(g_deco, 2, 'Blt_G_Deco', bool).
c_fct_name(g_dec, 2, 'Blt_G_Dec_2', bool).
c_fct_name(g_dec, 3, 'Blt_G_Dec_3', bool).
c_fct_name(g_set_bit, 2, 'Blt_G_Set_Bit', void).
c_fct_name(g_reset_bit, 2, 'Blt_G_Reset_Bit', void).
c_fct_name(g_test_set_bit, 2, 'Blt_G_Test_Set_Bit', bool).
c_fct_name(g_test_reset_bit, 2, 'Blt_G_Test_Reset_Bit', bool).
