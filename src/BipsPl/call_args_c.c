/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : call_args_c.c                                                   *
 * Descr.: meta call management - C part                                   *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2007 Daniel Diaz                                     *
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

#define OBJ_INIT Call_Args_Initializer

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

static int atom_call_with_args;
static int atom_call;




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

#define CALL_INTERNAL              X2463616C6C5F696E7465726E616C

Prolog_Prototype(CALL_INTERNAL, 2);




/*-------------------------------------------------------------------------*
 * CALL_ARGS_INITIALIZER                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Call_Args_Initializer(void)
{
  atom_call_with_args = Create_Atom("call_with_args");
  atom_call = Create_Atom("call");
}




/*-------------------------------------------------------------------------*
 * CALL_CLOSURE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static WamCont
Call_Closure(int atom_bip, int arity_rest)
{
  int call_info;
  int func, arity0, arity;
  WamWord *arg_adr;
  PredInf *pred;
  WamWord *w;
  int i;

  Set_C_Bip_Name(atom_tbl[atom_bip].name, 1 + arity_rest);
  if (atom_bip == atom_call_with_args) {
    func = Rd_Atom_Check(A(0));
    arity0 = 0;
  } else {
    arg_adr = Rd_Callable_Check(A(0), &func, &arity0);
  }
  
  arity = arity0 + arity_rest;
  if (arity > MAX_ARITY)
    Pl_Err_Representation(representation_max_arity);


  call_info = Call_Info(func, arity, 1);

  if ((pred = Lookup_Pred(func, arity)) == NULL)
    {
      if (arity > 0)
	{
	  w = H;
	  A(0) = Tag_STC(w);
	  *w++ = Functor_Arity(func, arity);
	  while(arity0-- > 0)
	    *w++ = *arg_adr++;
	  for (i = 1; i <= arity_rest; i++)
	    *w++ = A(i);
	  H = w;
	}

      A(1) = Tag_INT(Call_Info(atom_bip, arity_rest + 1, 1));

      return (CodePtr) Prolog_Predicate(CALL_INTERNAL, 2);
    }

  w = &A(0);
  while(arity0-- > 0)
    *w++ = *arg_adr++;
  for (i = 1; i <= arity_rest; i++)
    *w++ = A(i);

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
  return Call_Closure(atom_call_with_args, 0);
}

WamCont
Call_With_Args_2(void)
{
  return Call_Closure(atom_call_with_args, 1);
}

WamCont
Call_With_Args_3(void)
{
  return Call_Closure(atom_call_with_args, 2);
}

WamCont
Call_With_Args_4(void)
{
  return Call_Closure(atom_call_with_args, 3);
}

WamCont
Call_With_Args_5(void)
{
  return Call_Closure(atom_call_with_args, 4);
}

WamCont
Call_With_Args_6(void)
{
  return Call_Closure(atom_call_with_args, 5);
}

WamCont
Call_With_Args_7(void)
{
  return Call_Closure(atom_call_with_args, 6);
}

WamCont
Call_With_Args_8(void)
{
  return Call_Closure(atom_call_with_args, 7);
}

WamCont
Call_With_Args_9(void)
{
  return Call_Closure(atom_call_with_args, 8);
}

WamCont
Call_With_Args_10(void)
{
  return Call_Closure(atom_call_with_args, 9);
}

WamCont
Call_With_Args_11(void)
{
  return Call_Closure(atom_call_with_args, 10);
}


WamCont
Call_2(void)
{
  return Call_Closure(atom_call, 1);
}

WamCont
Call_3(void)
{
  return Call_Closure(atom_call, 2);
}

WamCont
Call_4(void)
{
  return Call_Closure(atom_call, 3);
}


WamCont
Call_5(void)
{
  return Call_Closure(atom_call, 4);
}

WamCont
Call_6(void)
{
  return Call_Closure(atom_call, 5);
}

WamCont
Call_7(void)
{
  return Call_Closure(atom_call, 6);
}

WamCont
Call_8(void)
{
  return Call_Closure(atom_call, 7);
}

WamCont
Call_9(void)
{
  return Call_Closure(atom_call, 8);
}

WamCont
Call_10(void)
{
  return Call_Closure(atom_call, 9);
}


WamCont
Call_11(void)
{
  return Call_Closure(atom_call, 10);
}
