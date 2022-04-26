/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : pred.c                                                          *
 * Descr.: predicate table management                                      *
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

#include <stdlib.h>
#include <string.h>


#define PRED_FILE

#include "engine_pl.h"




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




/*-------------------------------------------------------------------------*
 * INIT_PRED                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Init_Pred(void)
{
  pred_tbl = Hash_Alloc_Table(START_PRED_TBL_SIZE, sizeof(PredInf));
}




/*-------------------------------------------------------------------------*
 * CREATE_PRED                                                             *
 *                                                                         *
 * Called by compiled prolog code, by dynamic predicate support and by     *
 * byte-code support.                                                      *
 *-------------------------------------------------------------------------*/
PredInf * FC
Create_Pred_Module(int module, int marity,
		   int func, int arity, int pl_file, int pl_line,
		   int prop, long *codep)
{
  PredInf pred_info;
  PredInf *pred;
  AtomInf *atom_module;
  char **p_tbl;
  long key = Functor_Arity(func, arity);

#ifdef DEBUG
  DBGPRINTF("Create pred: %s/%d  unit: %s/%d  prop: %x\n",
	    atom_tbl[func].name, arity,
	    module>0? (atom_tbl[module].name): "<NONE>", marity,
	    prop);
#endif

  if (module < 0 || module == ATOM_NIL || module == atom_void)
    p_tbl = &pred_tbl;
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

  Extend_Table_If_Needed(p_tbl);
  pred = (PredInf *) Hash_Insert(*p_tbl, (char *) &pred_info, FALSE);

  return pred;
}




/*-------------------------------------------------------------------------*
 * CREATE_PRED                                                             *
 *                                                                         *
 * Called by compiled prolog code, by dynamic predicate support and by     *
 * byte-code support.                                                      *
 *-------------------------------------------------------------------------*/
PredInf * FC
Create_Pred(int func, int arity, int pl_file, int pl_line, int prop,
	    long *codep)
{
  return Create_Pred_Module(-1, 0, func, arity, pl_file, pl_line, prop, codep);
}


/* --- TO DO: Lookup_Pred_Module and Delete_Pred_Module --- */

PredInf * FC
Lookup_Pred_in_Cxt(int func, int arity, WamWord cxt)
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
Lookup_Pred(int func, int arity)
{
  long key = Functor_Arity(func, arity);
  PredInf *p = (PredInf *) Hash_Find(pred_tbl, key);

  if (p)
    return p;
  else {
    X(254) = X(255);
    return (PredInf *) Lookup_Pred_in_Cxt (func, arity, X(255));
  }
}




/*-------------------------------------------------------------------------*
 * DELETE_PRED                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Delete_Pred(int func, int arity)
{
  long key = Functor_Arity(func, arity);

  Hash_Delete(pred_tbl, key);
}
