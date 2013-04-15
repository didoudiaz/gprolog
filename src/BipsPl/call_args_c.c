/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : call_args_c.c                                                   *
 * Descr.: meta call management - C part                                   *
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
#include <string.h>


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

#define CALL_INTERNAL              X1_2463616C6C5F696E7465726E616C

Prolog_Prototype(CALL_INTERNAL, 2);




/*-------------------------------------------------------------------------*
 * CALL_ARGS_INITIALIZER                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
static void
Call_Args_Initializer(void)
{
  atom_call_with_args = Pl_Create_Atom("call_with_args");
  atom_call = Pl_Create_Atom("call");
}




/*-------------------------------------------------------------------------*
 * CALL_CLOSURE                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamCont
Pl_Call_Closure(int atom_bip, int arity_rest)
{
  int func, arity_clos, arity;
  WamWord *arg_adr;
  PredInf *pred;
  WamWord *w;
  int i;

  Pl_Set_C_Bip_Name(pl_atom_tbl[atom_bip].name, 1 + arity_rest);
  if (atom_bip == atom_call_with_args) {
    func = Pl_Rd_Atom_Check(A(0));
    arity_clos = 0;
  } else {
    arg_adr = Pl_Rd_Callable_Check(A(0), &func, &arity_clos);
  }

  arity = arity_clos + arity_rest;
  if (arity > MAX_ARITY)
    Pl_Err_Representation(pl_representation_max_arity);


  Pl_Unset_C_Bip_Name();

  if ((pred = Pl_Lookup_Pred(func, arity)) == NULL || 
      (pred->prop & MASK_PRED_CONTROL_CONSTRUCT))
    {
      if (arity > 0)
	{
	  w = H;
	  A(0) = Tag_STC(w);
	  *w++ = Functor_Arity(func, arity);
	  while(arity_clos-- > 0)
	    *w++ = *arg_adr++;
	  for (i = 1; i <= arity_rest; i++)
	    *w++ = A(i);
	  H = w;
	}

      A(1) = Tag_INT(Call_Info(atom_bip, arity_rest + 1, 1));

      return (CodePtr) Prolog_Predicate(CALL_INTERNAL, 2);
    }


  /* arity = arity_clos + arity_rest */
  /* the arity_clos args in the compound term   go in A(0)..A(arity_clos-1) */
  /* the arity_rest args in A(1)..A(arity_rest) go in A(arity_clos)..A(arity-1) */

  /* first copy the arity_rest args (to avoid the overwrite them copying closure args first) */
  /* NB: if arity_clos == 0 then dst < src */
  /*     if arity_clos == 1 then dst == src (optim) */
  /*     if arity_clos >= 2 then dst > src */
  /* we use memmove */

  if (arity_clos != 1)		/* optim: no copy needed when closure has 1 arg */
    memmove((void *) &A(arity_clos), &A(1), sizeof(WamWord) * arity_rest);

  /* then copy the arity_clos args */
  w = &A(0);
  while(arity_clos-- > 0)
    *w++ = *arg_adr++;

  if (pred->prop & MASK_PRED_NATIVE_CODE)	/* native code */
    return (WamCont) (pred->codep);

  return Pl_BC_Emulate_Pred(func, (DynPInf *) (pred->dyn));
}
