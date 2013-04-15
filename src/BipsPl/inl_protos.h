/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : inl_protos.h                                                    *
 * Descr.: inline predicate prototypes - header file                       *
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


/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

	  /* from type_inl_c.c */

Bool FC Pl_Blt_Var(WamWord x);

Bool FC Pl_Blt_Non_Var(WamWord x);

Bool FC Pl_Blt_Atom(WamWord x);

Bool FC Pl_Blt_Integer(WamWord x);

Bool FC Pl_Blt_Float(WamWord x);

Bool FC Pl_Blt_Number(WamWord x);

Bool FC Pl_Blt_Atomic(WamWord x);

Bool FC Pl_Blt_Compound(WamWord x);

Bool FC Pl_Blt_Callable(WamWord x);

Bool FC Pl_Blt_Ground(WamWord x);

Bool FC Pl_Blt_Fd_Var(WamWord x);

Bool FC Pl_Blt_Non_Fd_Var(WamWord x);

Bool FC Pl_Blt_Generic_Var(WamWord x);

Bool FC Pl_Blt_Non_Generic_Var(WamWord x);

Bool FC Pl_Blt_List(WamWord x);

Bool FC Pl_Blt_Partial_List(WamWord x);

Bool FC Pl_Blt_List_Or_Partial_List(WamWord x);



	  /* from term_inl_c.c */

Bool FC Pl_Blt_Term_Eq(WamWord x, WamWord y);

Bool FC Pl_Blt_Term_Neq(WamWord x, WamWord y);

Bool FC Pl_Blt_Term_Lt(WamWord x, WamWord y);

Bool FC Pl_Blt_Term_Lte(WamWord x, WamWord y);

Bool FC Pl_Blt_Term_Gt(WamWord x, WamWord y);

Bool FC Pl_Blt_Term_Gte(WamWord x, WamWord y);



Bool FC Pl_Blt_Compare(WamWord cmp_word, WamWord x, WamWord y);

Bool FC Pl_Blt_Arg(WamWord arg_no_word, WamWord term_word, WamWord sub_term_word);

Bool FC Pl_Blt_Functor(WamWord term_word, WamWord functor_word,
		    WamWord arity_word);

Bool FC Pl_Blt_Univ(WamWord term_word, WamWord list_word);



	  /* from g_var_inl_c.c */

void FC Pl_Blt_G_Assign(WamWord x, WamWord y);

void FC Pl_Blt_G_Assignb(WamWord x, WamWord y);

void FC Pl_Blt_G_Link(WamWord x, WamWord y);

Bool FC Pl_Blt_G_Read(WamWord x, WamWord y);

Bool FC Pl_Blt_G_Array_Size(WamWord x, WamWord y);

void FC Pl_Blt_G_Inc(WamWord x);

Bool FC Pl_Blt_G_Inco(WamWord x, WamWord y);

Bool FC Pl_Blt_G_Inc_2(WamWord x, WamWord y);

Bool FC Pl_Blt_G_Inc_3(WamWord x, WamWord y, WamWord z);

void FC Pl_Blt_G_Dec(WamWord x);

Bool FC Pl_Blt_G_Deco(WamWord x, WamWord y);

Bool FC Pl_Blt_G_Dec_2(WamWord x, WamWord y);

Bool FC Pl_Blt_G_Dec_3(WamWord x, WamWord y, WamWord z);

void FC Pl_Blt_G_Set_Bit(WamWord x, WamWord y);

void FC Pl_Blt_G_Reset_Bit(WamWord x, WamWord y);

Bool FC Pl_Blt_G_Test_Set_Bit(WamWord x, WamWord y);

Bool FC Pl_Blt_G_Test_Reset_Bit(WamWord x, WamWord y);




	  /* from arith_inl_c.c */

void FC Pl_Math_Fast_Load_Value(WamWord start_word, WamWord *word_adr);

void FC Pl_Math_Load_Value(WamWord start_word, WamWord *word_adr);

WamWord FC Pl_Fct_Fast_Neg(WamWord x);

WamWord FC Pl_Fct_Fast_Inc(WamWord x);

WamWord FC Pl_Fct_Fast_Dec(WamWord x);

WamWord FC Pl_Fct_Fast_Add(WamWord x, WamWord y);

WamWord FC Pl_Fct_Fast_Sub(WamWord x, WamWord y);

WamWord FC Pl_Fct_Fast_Mul(WamWord x, WamWord y);

WamWord FC Pl_Fct_Fast_Div(WamWord x, WamWord y);

WamWord FC Pl_Fct_Fast_Rem(WamWord x, WamWord y);

WamWord FC Pl_Fct_Fast_Mod(WamWord x, WamWord y);

WamWord FC Pl_Fct_Fast_Div2(WamWord x, WamWord y);

WamWord FC Pl_Fct_Fast_And(WamWord x, WamWord y);

WamWord FC Pl_Fct_Fast_Or(WamWord x, WamWord y);

WamWord FC Pl_Fct_Fast_Xor(WamWord x, WamWord y);

WamWord FC Pl_Fct_Fast_Not(WamWord x);

WamWord FC Pl_Fct_Fast_Shl(WamWord x, WamWord y);

WamWord FC Pl_Fct_Fast_Shr(WamWord x, WamWord y);

WamWord FC Pl_Fct_Fast_LSB(WamWord x);

WamWord FC Pl_Fct_Fast_MSB(WamWord x);

WamWord FC Pl_Fct_Fast_Popcount(WamWord x);

WamWord FC Pl_Fct_Fast_Abs(WamWord x);

WamWord FC Pl_Fct_Fast_Sign(WamWord x);

WamWord FC Pl_Fct_Fast_GCD(WamWord b, WamWord x);

WamWord FC Pl_Fct_Fast_Log_Radix(WamWord b, WamWord x);




WamWord FC Pl_Fct_Neg(WamWord x);

WamWord FC Pl_Fct_Inc(WamWord x);

WamWord FC Pl_Fct_Dec(WamWord x);

WamWord FC Pl_Fct_Add(WamWord x, WamWord y);

WamWord FC Pl_Fct_Sub(WamWord x, WamWord y);

WamWord FC Pl_Fct_Mul(WamWord x, WamWord y);

WamWord FC Pl_Fct_Div(WamWord x, WamWord y);

WamWord FC Pl_Fct_Float_Div(WamWord x, WamWord y);

WamWord FC Pl_Fct_Rem(WamWord x, WamWord y);

WamWord FC Pl_Fct_Mod(WamWord x, WamWord y);

WamWord FC Pl_Fct_Div2(WamWord x, WamWord y);

WamWord FC Pl_Fct_And(WamWord x, WamWord y);

WamWord FC Pl_Fct_Or(WamWord x, WamWord y);

WamWord FC Pl_Fct_Xor(WamWord x, WamWord y);

WamWord FC Pl_Fct_Not(WamWord x);

WamWord FC Pl_Fct_Shl(WamWord x, WamWord y);

WamWord FC Pl_Fct_Shr(WamWord x, WamWord y);

WamWord FC Pl_Fct_Abs(WamWord x);

WamWord FC Pl_Fct_LSB(WamWord x);

WamWord FC Pl_Fct_MSB(WamWord x);

WamWord FC Pl_Fct_Popcount(WamWord x);

WamWord FC Pl_Fct_Sign(WamWord x);



WamWord FC Pl_Fct_GCD(WamWord x, WamWord y);

WamWord FC Pl_Fct_Min(WamWord x, WamWord y);

WamWord FC Pl_Fct_Max(WamWord x, WamWord y);

WamWord FC Pl_Fct_Integer_Pow(WamWord x, WamWord y);

WamWord FC Pl_Fct_Pow(WamWord x, WamWord y);

WamWord FC Pl_Fct_Sqrt(WamWord x);

WamWord FC Pl_Fct_Tan(WamWord x);

WamWord FC Pl_Fct_Atan(WamWord x);

WamWord FC Pl_Fct_Atan2(WamWord x, WamWord y);

WamWord FC Pl_Fct_Cos(WamWord x);

WamWord FC Pl_Fct_Acos(WamWord x);

WamWord FC Pl_Fct_Sin(WamWord x);

WamWord FC Pl_Fct_Asin(WamWord x);

WamWord FC Pl_Fct_Tanh(WamWord x);

WamWord FC Pl_Fct_Atanh(WamWord x);

WamWord FC Pl_Fct_Cosh(WamWord x);

WamWord FC Pl_Fct_Acosh(WamWord x);

WamWord FC Pl_Fct_Sinh(WamWord x);

WamWord FC Pl_Fct_Asinh(WamWord x);

WamWord FC Pl_Fct_Exp(WamWord x);

WamWord FC Pl_Fct_Log(WamWord x);

WamWord FC Pl_Fct_Log10(WamWord x);

WamWord FC Pl_Fct_Log_Radix(WamWord b, WamWord x);

WamWord FC Pl_Fct_Float(WamWord x);

WamWord FC Pl_Fct_Ceiling(WamWord x);

WamWord FC Pl_Fct_Floor(WamWord x);

WamWord FC Pl_Fct_Round(WamWord x);

WamWord FC Pl_Fct_Truncate(WamWord x);

WamWord FC Pl_Fct_Float_Fract_Part(WamWord x);

WamWord FC Pl_Fct_Float_Integ_Part(WamWord x);

WamWord FC Pl_Fct_Identity(WamWord x);

WamWord FC Pl_Fct_PI(void);

WamWord FC Pl_Fct_E(void);

WamWord FC Pl_Fct_Epsilon(void);




Bool FC Pl_Blt_Fast_Eq(WamWord x, WamWord y);

Bool FC Pl_Blt_Fast_Neq(WamWord x, WamWord y);

Bool FC Pl_Blt_Fast_Lt(WamWord x, WamWord y);

Bool FC Pl_Blt_Fast_Lte(WamWord x, WamWord y);

Bool FC Pl_Blt_Fast_Gt(WamWord x, WamWord y);

Bool FC Pl_Blt_Fast_Gte(WamWord x, WamWord y);

Bool FC Pl_Blt_Eq(WamWord x, WamWord y);

Bool FC Pl_Blt_Neq(WamWord x, WamWord y);

Bool FC Pl_Blt_Lt(WamWord x, WamWord y);

Bool FC Pl_Blt_Lte(WamWord x, WamWord y);

Bool FC Pl_Blt_Gt(WamWord x, WamWord y);

Bool FC Pl_Blt_Gte(WamWord x, WamWord y);
