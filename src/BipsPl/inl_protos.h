/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : inl_protos.h                                                    *
 * Descr.: inline predicate prototypes - header file                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2005 Daniel Diaz                                     *
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
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.               *
 *-------------------------------------------------------------------------*/

/* $Id$ */

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

Bool FC Blt_Var(WamWord x);

Bool FC Blt_Non_Var(WamWord x);

Bool FC Blt_Atom(WamWord x);

Bool FC Blt_Integer(WamWord x);

Bool FC Blt_Float(WamWord x);

Bool FC Blt_Number(WamWord x);

Bool FC Blt_Atomic(WamWord x);

Bool FC Blt_Compound(WamWord x);

Bool FC Blt_Callable(WamWord x);

Bool FC Blt_Fd_Var(WamWord x);

Bool FC Blt_Non_Fd_Var(WamWord x);

Bool FC Blt_Generic_Var(WamWord x);

Bool FC Blt_Non_Generic_Var(WamWord x);

Bool FC Blt_List(WamWord x);

Bool FC Blt_Partial_List(WamWord x);

Bool FC Blt_List_Or_Partial_List(WamWord x);



	  /* from term_inl_c.c */

Bool FC Blt_Term_Eq(WamWord x, WamWord y);

Bool FC Blt_Term_Neq(WamWord x, WamWord y);

Bool FC Blt_Term_Lt(WamWord x, WamWord y);

Bool FC Blt_Term_Lte(WamWord x, WamWord y);

Bool FC Blt_Term_Gt(WamWord x, WamWord y);

Bool FC Blt_Term_Gte(WamWord x, WamWord y);



Bool FC Blt_Compare(WamWord cmp_word, WamWord x, WamWord y);

Bool FC Blt_Arg(WamWord arg_no_word, WamWord term_word, WamWord sub_term_word);

Bool FC Blt_Functor(WamWord term_word, WamWord functor_word,
		    WamWord arity_word);

Bool FC Blt_Univ(WamWord term_word, WamWord list_word);



	  /* from g_var_inl_c.c */

void FC Blt_G_Assign(WamWord x, WamWord y);

void FC Blt_G_Assignb(WamWord x, WamWord y);

void FC Blt_G_Link(WamWord x, WamWord y);

Bool FC Blt_G_Read(WamWord x, WamWord y);

Bool FC Blt_G_Array_Size(WamWord x, WamWord y);

void FC Blt_G_Inc(WamWord x);

Bool FC Blt_G_Inco(WamWord x, WamWord y);

Bool FC Blt_G_Inc_2(WamWord x, WamWord y);

Bool FC Blt_G_Inc_3(WamWord x, WamWord y, WamWord z);

void FC Blt_G_Dec(WamWord x);

Bool FC Blt_G_Deco(WamWord x, WamWord y);

Bool FC Blt_G_Dec_2(WamWord x, WamWord y);

Bool FC Blt_G_Dec_3(WamWord x, WamWord y, WamWord z);

void FC Blt_G_Set_Bit(WamWord x, WamWord y);

void FC Blt_G_Reset_Bit(WamWord x, WamWord y);

Bool FC Blt_G_Test_Set_Bit(WamWord x, WamWord y);

Bool FC Blt_G_Test_Reset_Bit(WamWord x, WamWord y);




	  /* from arith_inl_c.c */

void FC Math_Fast_Load_Value(WamWord start_word, WamWord *word_adr);

void FC Math_Load_Value(WamWord start_word, WamWord *word_adr);

WamWord FC Fct_Fast_Neg(WamWord x);

WamWord FC Fct_Fast_Inc(WamWord x);

WamWord FC Fct_Fast_Dec(WamWord x);

WamWord FC Fct_Fast_Add(WamWord x, WamWord y);

WamWord FC Fct_Fast_Sub(WamWord x, WamWord y);

WamWord FC Fct_Fast_Mul(WamWord x, WamWord y);

WamWord FC Fct_Fast_Div(WamWord x, WamWord y);

WamWord FC Fct_Fast_Rem(WamWord x, WamWord y);

WamWord FC Fct_Fast_Mod(WamWord x, WamWord y);

WamWord FC Fct_Fast_And(WamWord x, WamWord y);

WamWord FC Fct_Fast_Or(WamWord x, WamWord y);

WamWord FC Fct_Fast_Xor(WamWord x, WamWord y);

WamWord FC Fct_Fast_Not(WamWord x);

WamWord FC Fct_Fast_Shl(WamWord x, WamWord y);

WamWord FC Fct_Fast_Shr(WamWord x, WamWord y);

WamWord FC Fct_Fast_Abs(WamWord x);

WamWord FC Fct_Fast_Sign(WamWord x);

WamWord FC Fct_Neg(WamWord x);

WamWord FC Fct_Inc(WamWord x);

WamWord FC Fct_Dec(WamWord x);

WamWord FC Fct_Add(WamWord x, WamWord y);

WamWord FC Fct_Sub(WamWord x, WamWord y);

WamWord FC Fct_Mul(WamWord x, WamWord y);

WamWord FC Fct_Div(WamWord x, WamWord y);

WamWord FC Fct_Float_Div(WamWord x, WamWord y);

WamWord FC Fct_Rem(WamWord x, WamWord y);

WamWord FC Fct_Mod(WamWord x, WamWord y);

WamWord FC Fct_And(WamWord x, WamWord y);

WamWord FC Fct_Or(WamWord x, WamWord y);

WamWord FC Fct_Xor(WamWord x, WamWord y);

WamWord FC Fct_Not(WamWord x);

WamWord FC Fct_Shl(WamWord x, WamWord y);

WamWord FC Fct_Shr(WamWord x, WamWord y);

WamWord FC Fct_Abs(WamWord x);

WamWord FC Fct_Sign(WamWord x);



WamWord FC Fct_Min(WamWord x, WamWord y);

WamWord FC Fct_Max(WamWord x, WamWord y);

WamWord FC Fct_Pow(WamWord x, WamWord y);

WamWord FC Fct_Sqrt(WamWord x);

WamWord FC Fct_Atan(WamWord x);

WamWord FC Fct_Cos(WamWord x);

WamWord FC Fct_Acos(WamWord x);

WamWord FC Fct_Sin(WamWord x);

WamWord FC Fct_Asin(WamWord x);

WamWord FC Fct_Exp(WamWord x);

WamWord FC Fct_Log(WamWord x);

WamWord FC Fct_Float(WamWord x);

WamWord FC Fct_Ceiling(WamWord x);

WamWord FC Fct_Floor(WamWord x);

WamWord FC Fct_Round(WamWord x);

WamWord FC Fct_Truncate(WamWord x);

WamWord FC Fct_Float_Fract_Part(WamWord x);

WamWord FC Fct_Float_Integ_Part(WamWord x);

WamWord FC Fct_Identity(WamWord x);

Bool FC Blt_Fast_Eq(WamWord x, WamWord y);

Bool FC Blt_Fast_Neq(WamWord x, WamWord y);

Bool FC Blt_Fast_Lt(WamWord x, WamWord y);

Bool FC Blt_Fast_Lte(WamWord x, WamWord y);

Bool FC Blt_Fast_Gt(WamWord x, WamWord y);

Bool FC Blt_Fast_Gte(WamWord x, WamWord y);

Bool FC Blt_Eq(WamWord x, WamWord y);

Bool FC Blt_Neq(WamWord x, WamWord y);

Bool FC Blt_Lt(WamWord x, WamWord y);

Bool FC Blt_Lte(WamWord x, WamWord y);

Bool FC Blt_Gt(WamWord x, WamWord y);

Bool FC Blt_Gte(WamWord x, WamWord y);
