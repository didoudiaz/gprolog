/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : pred.c                                                          *
 * Descr.: predicate table management                                      *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2022 Daniel Diaz                                     *
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
Pl_Create_Pred_Module(int module, int marity,
		   int func, int arity, int pl_file, int pl_line,
		   int prop, long *codep)
{
  PredInf pred_info;
  PredInf *pred;
  AtomInf *atom_module;
  char **p_tbl;
  PlLong key = Functor_Arity(func, arity);

  if (prop & (MASK_PRED_BUILTIN_FD | MASK_PRED_CONTROL_CONSTRUCT))
    prop |= MASK_PRED_BUILTIN;	/* now an FD built-in or a CC is also a built-in */

#ifdef DEBUG
  DBGPRINTF("Create pred: %s/%d  unit: %s/%d  prop: %x\n",
	    atom_tbl[func].name, arity,
	    module>0? (atom_tbl[module].name): "<NONE>", marity,
	    prop);
#endif

  if (module < 0 || module == ATOM_NIL || module == atom_void)
    p_tbl = &pl_pred_tbl;
  else
    {				/* lookup or create a module pred table */
      atom_module = atom_tbl + module;
      
      if (atom_module->modules == NULL) /* no units yet */
	atom_module->modules = (void *) Calloc (256, (sizeof (void *)));

#if 0				/* cannot happen because of max_arity */
      if (marity > 255)
	Fatal_Error("Fatal error: attemted unit with arity %d!", marity);
#endif

      if (atom_module->modules[marity] == NULL)
	atom_module->modules[marity] =
	  Hash_Alloc_Table(START_MODULE_PRED_TBL_SIZE, sizeof(PredInf));
      p_tbl = &atom_module->modules[marity];
    }

  pred_info.f_n = key;
  pred_info.prop = prop;
  pred_info.pl_file = pl_file;
  pred_info.pl_line = pl_line;
  pred_info.codep = codep;
  pred_info.dyn = NULL;

  Pl_Extend_Table_If_Needed(p_tbl);
  pred = (PredInf *) Hash_Insert(*p_tbl, (char *) &pred_info, FALSE);

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
 * CREATE_PRED                                                             *
 *                                                                         *
 * Called by compiled prolog code, by dynamic predicate support and by     *
 * byte-code support.                                                      *
 *-------------------------------------------------------------------------*/
PredInf * FC
Pl_Create_Pred(int func, int arity, int pl_file, int pl_line, int prop,
	    long *codep)
{
  return Pl_Create_Pred_Module(-1, 0, 
			       func, arity, pl_file, pl_line, prop, codep);
}


/* --- TO DO: Lookup_Pred_Module and Delete_Pred_Module --- */

PredInf * FC
Pl_Lookup_Pred_in_Cxt(int func, int arity, WamWord cxt)
{
  long key = Functor_Arity(func, arity);
  extern PredInf *Cxt_Lookup_Pred_With_K(WamWord, WamWord) FC;

  return (PredInf *) Cxt_Lookup_Pred_With_K(key, cxt);
}

/*-------------------------------------------------------------------------*
 * LOOKUP_PRED                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PredInf * FC
Pl_Lookup_Pred(int func, int arity)
{
  long key = Functor_Arity(func, arity);
  PredInf *p = (PredInf *) Pl_Hash_Find(pl_pred_tbl, key);

  if (p)			/* global predicate */
    return p;
  else {			/* predicate in context -> search */
    X(254) = X(255);
    return (PredInf *) Lookup_Pred_in_Cxt (func, arity, X(255));
  }
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
