/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : inl_protos.h                                                    *
 * Descr.: inline predicate prototypes - header file                       *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999,2000 Daniel Diaz                                     *
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

Bool Blt_Var(WamWord x);

Bool Blt_Non_Var(WamWord x);

Bool Blt_Atom(WamWord x);

Bool Blt_Integer(WamWord x);

Bool Blt_Float(WamWord x);

Bool Blt_Number(WamWord x);

Bool Blt_Atomic(WamWord x);

Bool Blt_Compound(WamWord x);

Bool Blt_Callable(WamWord x);

Bool Blt_Fd_Var(WamWord x);

Bool Blt_Non_Fd_Var(WamWord x);

Bool Blt_Generic_Var(WamWord x);

Bool Blt_Non_Generic_Var(WamWord x);

Bool Blt_List(WamWord x);

Bool Blt_Partial_List(WamWord x);

Bool Blt_List_Or_Partial_List(WamWord x);



	  /* from term_inl_c.c */

Bool Blt_Term_Eq(WamWord x, WamWord y);

Bool Blt_Term_Neq(WamWord x, WamWord y);

Bool Blt_Term_Lt(WamWord x, WamWord y);

Bool Blt_Term_Lte(WamWord x, WamWord y);

Bool Blt_Term_Gt(WamWord x, WamWord y);

Bool Blt_Term_Gte(WamWord x, WamWord y);



Bool Blt_Compare(WamWord cmp_word, WamWord x, WamWord y);

Bool Blt_Arg(WamWord arg_no_word, WamWord term_word, WamWord sub_term_word);

Bool Blt_Functor(WamWord term_word, WamWord functor_word,
		 WamWord arity_word);

Bool Blt_Univ(WamWord term_word, WamWord list_word);



	  /* from g_var_inl_c.c */

Bool Blt_G_Assign(WamWord x, WamWord y);

Bool Blt_G_Assignb(WamWord x, WamWord y);

Bool Blt_G_Link(WamWord x, WamWord y);

Bool Blt_G_Read(WamWord x, WamWord y);

Bool Blt_G_Array_Size(WamWord x, WamWord y);



	  /* from arith_inl_c.c */

void Math_Fast_Load_Value(WamWord start_word, WamWord *word_adr) FC;

void Math_Load_Value(WamWord start_word, WamWord *word_adr) FC;

WamWord Fct_Fast_Neg(int x);

WamWord Fct_Fast_Inc(int x);

WamWord Fct_Fast_Dec(int x);

WamWord Fct_Fast_Add(int x, int y);

WamWord Fct_Fast_Sub(int x, int y);

WamWord Fct_Fast_Mul(int x, int y);

WamWord Fct_Fast_Div(int x, int y);

WamWord Fct_Fast_Rem(int x, int y);

WamWord Fct_Fast_Mod(int x, int y);

WamWord Fct_Fast_And(int x, int y);

WamWord Fct_Fast_Or(int x, int y);

WamWord Fct_Fast_Xor(int x, int y);

WamWord Fct_Fast_Not(int x);

WamWord Fct_Fast_Shl(int x, int y);

WamWord Fct_Fast_Shr(int x, int y);

WamWord Fct_Fast_Abs(int x);

WamWord Fct_Fast_Sign(int x);

WamWord Fct_Neg(WamWord x);

WamWord Fct_Inc(WamWord x);

WamWord Fct_Dec(WamWord x);

WamWord Fct_Add(WamWord x, WamWord y);

WamWord Fct_Sub(WamWord x, WamWord y);

WamWord Fct_Mul(WamWord x, WamWord y);

WamWord Fct_Div(WamWord x, WamWord y);

WamWord Fct_Float_Div(WamWord x, WamWord y);

WamWord Fct_Rem(WamWord x, WamWord y);

WamWord Fct_Mod(WamWord x, WamWord y);

WamWord Fct_And(WamWord x, WamWord y);

WamWord Fct_Or(WamWord x, WamWord y);

WamWord Fct_Xor(WamWord x, WamWord y);

WamWord Fct_Not(WamWord x);

WamWord Fct_Shl(WamWord x, WamWord y);

WamWord Fct_Shr(WamWord x, WamWord y);

WamWord Fct_Abs(WamWord x);

WamWord Fct_Sign(WamWord x);



WamWord Fct_Pow(WamWord x, WamWord y);

WamWord Fct_Sqrt(WamWord x);

WamWord Fct_Atan(WamWord x);

WamWord Fct_Cos(WamWord x);

WamWord Fct_Sin(WamWord x);

WamWord Fct_Exp(WamWord x);

WamWord Fct_Log(WamWord x);

WamWord Fct_Float(WamWord x);

WamWord Fct_Ceiling(WamWord x);

WamWord Fct_Floor(WamWord x);

WamWord Fct_Round(WamWord x);

WamWord Fct_Truncate(WamWord x);

WamWord Fct_Float_Fract_Part(WamWord x);

WamWord Fct_Float_Integ_Part(WamWord x);

Bool Blt_Fast_Eq(WamWord x, WamWord y);

Bool Blt_Fast_Neq(WamWord x, WamWord y);

Bool Blt_Fast_Lt(WamWord x, WamWord y);

Bool Blt_Fast_Lte(WamWord x, WamWord y);

Bool Blt_Fast_Gt(WamWord x, WamWord y);

Bool Blt_Fast_Gte(WamWord x, WamWord y);

Bool Blt_Eq(WamWord x, WamWord y);

Bool Blt_Neq(WamWord x, WamWord y);

Bool Blt_Lt(WamWord x, WamWord y);

Bool Blt_Lte(WamWord x, WamWord y);

Bool Blt_Gt(WamWord x, WamWord y);

Bool Blt_Gte(WamWord x, WamWord y);
