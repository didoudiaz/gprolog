/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : pred.c                                                          *
 * Descr.: predicate table management                                      *
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


#include <stdlib.h>
#include <string.h>

#define PRED_FILE

#include "engine_pl.h"


/* define if CC are added to the predicate table - see pred.c */
#if 1
#define ADD_CONTROL_CONSTRUCTS_IN_PRED_TBL
#endif


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
#ifdef ADD_CONTROL_CONSTRUCTS_IN_PRED_TBL

  int file = Pl_Create_Atom(__FILE__);
  int prop = MASK_PRED_NATIVE_CODE | MASK_PRED_CONTROL_CONSTRUCT | MASK_PRED_EXPORTED;

#endif

  pl_pred_tbl = Pl_Hash_Alloc_Table(START_PRED_TBL_SIZE, sizeof(PredInf));

/* The following control constructs are defined as predicates ONLY to:
 *
 * - be found by current_predicate/1 (if strict_iso is off)
 * - be found by predicate_property/2
 * - prevent their redefinition (e.g. asserta/1 will raise a permission_error)
 *
 * NB: see ISO Core 1 Section 7.5 about what is a "procedure" 
 * (bult-in predicates, control constructs or user defined predicates).
 *
 * Anyway, these predicates should NEVER called. Ensure it ! 
 * Check the compiler, meta-calls (call/1) and the debugger...
 *
 * This file is ALWAYS linked (see EnginePl/pred.c).
 */

#ifdef ADD_CONTROL_CONSTRUCTS_IN_PRED_TBL

  Pl_Create_Pred(ATOM_CHAR(','), 2, file, __LINE__, prop, NULL);
  Pl_Create_Pred(ATOM_CHAR(';'), 2, file, __LINE__, prop, NULL);
  Pl_Create_Pred(Pl_Create_Atom("->"), 2, file, __LINE__, prop, NULL);
  Pl_Create_Pred(Pl_Create_Atom("*->"), 2, file, __LINE__, prop, NULL);
  Pl_Create_Pred(ATOM_CHAR('!'), 0, file, __LINE__, prop, NULL);
  Pl_Create_Pred(Pl_Create_Atom("fail"), 0, file, __LINE__, prop, NULL);
  Pl_Create_Pred(pl_atom_true, 0, file, __LINE__, prop, NULL);
  Pl_Create_Pred(Pl_Create_Atom("call"), 1, file, __LINE__, prop, NULL);
  Pl_Create_Pred(Pl_Create_Atom("catch"), 3, file, __LINE__, prop, NULL);
  Pl_Create_Pred(Pl_Create_Atom("throw"), 1, file, __LINE__, prop, NULL);

#endif
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


  if (prop & (MASK_PRED_BUILTIN_FD | MASK_PRED_CONTROL_CONSTRUCT))
    prop |= MASK_PRED_BUILTIN;	/* now an FD built-in or a CC is also a built-in */

#ifdef DEBUG
  DBGPRINTF("Create pred: %s/%d  prop: %x\n", pl_atom_tbl[func].name, arity, prop);
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
