/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : inl_protos.h                                                    *
 * Descr.: inline predicate prototypes - header file                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2002 Daniel Diaz                                     *
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

Bool Blt_Var(WamWord x) FC;

Bool Blt_Non_Var(WamWord x) FC;

Bool Blt_Atom(WamWord x) FC;

Bool Blt_Integer(WamWord x) FC;

Bool Blt_Float(WamWord x) FC;

Bool Blt_Number(WamWord x) FC;

Bool Blt_Atomic(WamWord x) FC;

Bool Blt_Compound(WamWord x) FC;

Bool Blt_Callable(WamWord x) FC;

Bool Blt_Fd_Var(WamWord x) FC;

Bool Blt_Non_Fd_Var(WamWord x) FC;

Bool Blt_Generic_Var(WamWord x) FC;

Bool Blt_Non_Generic_Var(WamWord x) FC;

Bool Blt_List(WamWord x) FC;

Bool Blt_Partial_List(WamWord x) FC;

Bool Blt_List_Or_Partial_List(WamWord x) FC;



	  /* from term_inl_c.c */

Bool Blt_Term_Eq(WamWord x, WamWord y) FC;

Bool Blt_Term_Neq(WamWord x, WamWord y) FC;

Bool Blt_Term_Lt(WamWord x, WamWord y) FC;

Bool Blt_Term_Lte(WamWord x, WamWord y) FC;

Bool Blt_Term_Gt(WamWord x, WamWord y) FC;

Bool Blt_Term_Gte(WamWord x, WamWord y) FC;



Bool Blt_Compare(WamWord cmp_word, WamWord x, WamWord y) FC;

Bool Blt_Arg(WamWord arg_no_word, WamWord term_word, WamWord sub_term_word) FC;

Bool Blt_Functor(WamWord term_word, WamWord functor_word,
		 WamWord arity_word) FC;

Bool Blt_Univ(WamWord term_word, WamWord list_word) FC;



	  /* from g_var_inl_c.c */

void Blt_G_Assign(WamWord x, WamWord y) FC;

void Blt_G_Assignb(WamWord x, WamWord y) FC;

void Blt_G_Link(WamWord x, WamWord y) FC;

Bool Blt_G_Read(WamWord x, WamWord y) FC;

Bool Blt_G_Array_Size(WamWord x, WamWord y) FC;

void Blt_G_Inc(WamWord x) FC;

Bool Blt_G_Inco(WamWord x, WamWord y) FC;

Bool Blt_G_Inc_2(WamWord x, WamWord y) FC;

Bool Blt_G_Inc_3(WamWord x, WamWord y, WamWord z) FC;

void Blt_G_Dec(WamWord x) FC;

Bool Blt_G_Deco(WamWord x, WamWord y) FC;

Bool Blt_G_Dec_2(WamWord x, WamWord y) FC;

Bool Blt_G_Dec_3(WamWord x, WamWord y, WamWord z) FC;

void Blt_G_Set_Bit(WamWord x, WamWord y) FC;

void Blt_G_Reset_Bit(WamWord x, WamWord y) FC;

Bool Blt_G_Test_Set_Bit(WamWord x, WamWord y) FC;

Bool Blt_G_Test_Reset_Bit(WamWord x, WamWord y) FC;




	  /* from arith_inl_c.c */

void Math_Fast_Load_Value(WamWord start_word, WamWord *word_adr) FC;

void Math_Load_Value(WamWord start_word, WamWord *word_adr) FC;

WamWord Fct_Fast_Neg(WamWord x) FC;

WamWord Fct_Fast_Inc(WamWord x) FC;

WamWord Fct_Fast_Dec(WamWord x) FC;

WamWord Fct_Fast_Add(WamWord x, WamWord y) FC;

WamWord Fct_Fast_Sub(WamWord x, WamWord y) FC;

WamWord Fct_Fast_Mul(WamWord x, WamWord y) FC;

WamWord Fct_Fast_Div(WamWord x, WamWord y) FC;

WamWord Fct_Fast_Rem(WamWord x, WamWord y) FC;

WamWord Fct_Fast_Mod(WamWord x, WamWord y) FC;

WamWord Fct_Fast_And(WamWord x, WamWord y) FC;

WamWord Fct_Fast_Or(WamWord x, WamWord y) FC;

WamWord Fct_Fast_Xor(WamWord x, WamWord y) FC;

WamWord Fct_Fast_Not(WamWord x) FC;

WamWord Fct_Fast_Shl(WamWord x, WamWord y) FC;

WamWord Fct_Fast_Shr(WamWord x, WamWord y) FC;

WamWord Fct_Fast_Abs(WamWord x) FC;

WamWord Fct_Fast_Sign(WamWord x) FC;

WamWord Fct_Neg(WamWord x) FC;

WamWord Fct_Inc(WamWord x) FC;

WamWord Fct_Dec(WamWord x) FC;

WamWord Fct_Add(WamWord x, WamWord y) FC;

WamWord Fct_Sub(WamWord x, WamWord y) FC;

WamWord Fct_Mul(WamWord x, WamWord y) FC;

WamWord Fct_Div(WamWord x, WamWord y) FC;

WamWord Fct_Float_Div(WamWord x, WamWord y) FC;

WamWord Fct_Rem(WamWord x, WamWord y) FC;

WamWord Fct_Mod(WamWord x, WamWord y) FC;

WamWord Fct_And(WamWord x, WamWord y) FC;

WamWord Fct_Or(WamWord x, WamWord y) FC;

WamWord Fct_Xor(WamWord x, WamWord y) FC;

WamWord Fct_Not(WamWord x) FC;

WamWord Fct_Shl(WamWord x, WamWord y) FC;

WamWord Fct_Shr(WamWord x, WamWord y) FC;

WamWord Fct_Abs(WamWord x) FC;

WamWord Fct_Sign(WamWord x) FC;



WamWord Fct_Min(WamWord x, WamWord y) FC;

WamWord Fct_Max(WamWord x, WamWord y) FC;

WamWord Fct_Pow(WamWord x, WamWord y) FC;

WamWord Fct_Sqrt(WamWord x) FC;

WamWord Fct_Atan(WamWord x) FC;

WamWord Fct_Cos(WamWord x) FC;

WamWord Fct_Acos(WamWord x) FC;

WamWord Fct_Sin(WamWord x) FC;

WamWord Fct_Asin(WamWord x) FC;

WamWord Fct_Exp(WamWord x) FC;

WamWord Fct_Log(WamWord x) FC;

WamWord Fct_Float(WamWord x) FC;

WamWord Fct_Ceiling(WamWord x) FC;

WamWord Fct_Floor(WamWord x) FC;

WamWord Fct_Round(WamWord x) FC;

WamWord Fct_Truncate(WamWord x) FC;

WamWord Fct_Float_Fract_Part(WamWord x) FC;

WamWord Fct_Float_Integ_Part(WamWord x) FC;

WamWord Fct_Identity(WamWord x) FC;

Bool Blt_Fast_Eq(WamWord x, WamWord y) FC;

Bool Blt_Fast_Neq(WamWord x, WamWord y) FC;

Bool Blt_Fast_Lt(WamWord x, WamWord y) FC;

Bool Blt_Fast_Lte(WamWord x, WamWord y) FC;

Bool Blt_Fast_Gt(WamWord x, WamWord y) FC;

Bool Blt_Fast_Gte(WamWord x, WamWord y) FC;

Bool Blt_Eq(WamWord x, WamWord y) FC;

Bool Blt_Neq(WamWord x, WamWord y) FC;

Bool Blt_Lt(WamWord x, WamWord y) FC;

Bool Blt_Lte(WamWord x, WamWord y) FC;

Bool Blt_Gt(WamWord x, WamWord y) FC;

Bool Blt_Gte(WamWord x, WamWord y) FC;
