/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : call_args_c.c                                                   *
 * Descr.: meta call management - C part                                   *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2004 Daniel Diaz                                     *
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

#include "engine_pl.h"
#include "bips_pl.h"


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

#define CALL_INTERNAL              X2463616C6C5F696E7465726E616C

Prolog_Prototype(CALL_INTERNAL, 2);




/*-------------------------------------------------------------------------*
 * CALL_WITH_ARGS                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamCont
Call_With_Args(int arity)
{
  int call_info;
  int func;
  PredInf *pred;
  WamWord *w;
  int i;

  Set_C_Bip_Name("call_with_args", 1 + arity);
  func = Rd_Atom_Check(A(0));

  call_info = Call_Info(func, arity, 1);

  if ((pred = Lookup_Pred(func, arity)) == NULL)
    {
      if (arity > 0)
	{
	  w = H;
	  A(0) = Tag_STC(w);
	  *w++ = Functor_Arity(func, arity);
	  for (i = 0; i < arity;)
	    *w++ = A(++i);
	  H = w;
	}

      A(1) =
	Tag_INT(
		  Call_Info(Create_Atom("call_with_args"), arity + 1, 1));
      return (CodePtr) Prolog_Predicate(CALL_INTERNAL, 2);
    }

  for (i = 0; i < arity; i++)
    A(i) = A(i + 1);

  if (pred->prop & MASK_PRED_NATIVE_CODE)	/* native code */
    return (WamCont) (pred->codep);

  return BC_Emulate_Pred(func, (DynPInf *) (pred->dyn));
}




/*-------------------------------------------------------------------------*
 * CALL_WITH_ARGS_...                                                      *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamCont
Call_With_Args_1(void)
{
  return Call_With_Args(0);
}

WamCont
Call_With_Args_2(void)
{
  return Call_With_Args(1);
}

WamCont
Call_With_Args_3(void)
{
  return Call_With_Args(2);
}

WamCont
Call_With_Args_4(void)
{
  return Call_With_Args(3);
}

WamCont
Call_With_Args_5(void)
{
  return Call_With_Args(4);
}

WamCont
Call_With_Args_6(void)
{
  return Call_With_Args(5);
}

WamCont
Call_With_Args_7(void)
{
  return Call_With_Args(6);
}

WamCont
Call_With_Args_8(void)
{
  return Call_With_Args(7);
}

WamCont
Call_With_Args_9(void)
{
  return Call_With_Args(8);
}

WamCont
Call_With_Args_10(void)
{
  return Call_With_Args(9);
}

WamCont
Call_With_Args_11(void)
{
  return Call_With_Args(10);
}
