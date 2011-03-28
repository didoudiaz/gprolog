/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : pred.c                                                          *
 * Descr.: predicate table management                                      *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2011 Daniel Diaz                                     *
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

/* $Id$ */

#include <stdlib.h>
#include <string.h>


#define PRED_FILE

#include "engine_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define ERR_MULTIFILE_PROP         "multifile predicate %s/%d not declared consistently\n    in  %s\n    and %s"





/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/




/*-------------------------------------------------------------------------*
 * PL_INIT_PRED                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Init_Pred(void)
{
  pl_pred_tbl = Pl_Hash_Alloc_Table(START_PRED_TBL_SIZE, sizeof(PredInf));
}




/*-------------------------------------------------------------------------*
 * PL_CREATE_PRED                                                          *
 *                                                                         *
 * Called by compiled prolog code, by dynamic predicate support and by     *
 * byte-code support.                                                      *
 *-------------------------------------------------------------------------*/
PredInf * FC
Pl_Create_Pred(int func, int arity, int pl_file, int pl_line, int prop,
	       PlLong *codep)
{
  PredInf pred_info;
  PredInf *pred;
  PlLong key = Functor_Arity(func, arity);


#ifdef DEBUG
  DBGPRINTF("Create pred: %s/%d  prop: %x\n", pl_atom_tbl[func].name, arity,
	    prop);
#endif

  pred_info.f_n = key;
  pred_info.prop = prop;
  pred_info.pl_file = pl_file;
  pred_info.pl_line = pl_line;
  pred_info.codep = codep;
  pred_info.dyn = NULL;

  Pl_Extend_Table_If_Needed(&pl_pred_tbl);
  pred = (PredInf *) Pl_Hash_Insert(pl_pred_tbl, (char *) &pred_info, FALSE);

  if (prop != pred->prop)	/* predicate exists - occurs for multifile pred */
    {
      Pl_Fatal_Error(ERR_MULTIFILE_PROP, pl_atom_tbl[func].name, arity,
		     pl_atom_tbl[pred->pl_file].name, pl_atom_tbl[pl_file].name);
      pred->prop = prop;
    }

#if 1				/* for multifile record the first file where it appears */
  pred->pl_file = pl_file;
  pred->pl_line = pl_line;
#endif

  return pred;
}




/*-------------------------------------------------------------------------*
 * PL_LOOKUP_PRED                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PredInf * FC
Pl_Lookup_Pred(int func, int arity)
{
  PlLong key = Functor_Arity(func, arity);

  return (PredInf *) Pl_Hash_Find(pl_pred_tbl, key);
}




/*-------------------------------------------------------------------------*
 * PL_DELETE_PRED                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Delete_Pred(int func, int arity)
{
  PlLong key = Functor_Arity(func, arity);

  Pl_Hash_Delete(pl_pred_tbl, key);
}
