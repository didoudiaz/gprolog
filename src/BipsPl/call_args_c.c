/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : call_args_c.c                                                   *
 * Descr.: meta call management - C part                                   *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2023 Daniel Diaz                                     *
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
  int caller_func = atom_bip;
  int caller_arity = arity_rest + 1;
  int module, func, arity_clos, arity;
  WamWord goal_word;
  WamWord *arg_adr = NULL;	/* init for the compiler */
#if 0				/* TODO check why it is so different from master branch */
  PredInf *pred;
  WamWord *w;
  int i;
#endif
  Pl_Set_C_Bip_Name(pl_atom_tbl[caller_func].name, caller_arity);

  module = Pl_Strip_Module_Top(A(0), FALSE, TRUE, &goal_word);

  if (atom_bip == atom_call_with_args) {
    func = Pl_Rd_Atom_Check(goal_word);
    arity_clos = 0;
  } else {
    arg_adr = Pl_Rd_Callable_Check(goal_word, &func, &arity_clos);
  }

  arity = arity_clos + arity_rest;
  if (arity > MAX_ARITY)
    Pl_Err_Representation(pl_representation_max_arity);


  Pl_Unset_C_Bip_Name();

  /* arity = arity_clos + arity_rest */
  /* the arity_clos args in the compound term   go in A(0)..A(arity_clos-1) */
  /* the arity_rest args in A(1)..A(arity_rest) go in A(arity_clos)..A(arity-1) */

  /* first copy the arity_rest args (to avoid the overwrite them copying closure args first) */
  /* NB: if arity_clos == 0 then dst < src */
  /*     if arity_clos == 1 then dst == src (optim) */
  /*     if arity_clos >= 2 then dst > src */
  /* we use memmove */

  if (arity_clos != 1)		/* optim: no copy needed when closure has 1 arg */
    memmove(&A(arity_clos), &A(1), sizeof(WamWord) * arity_rest);

  /* then copy the arity_clos args */
  memcpy((void *) &A(0), arg_adr, sizeof(WamWord) * arity_clos);

  return Pl_BC_Call_Initial(module, func, arity, &A(0), NOT_A_WAM_WORD, caller_func, caller_arity, TRUE);
}
