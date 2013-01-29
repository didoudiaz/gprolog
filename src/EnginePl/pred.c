/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : pred.c                                                          *
 * Descr.: module/predicate table management                               *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2012 Daniel Diaz                                     *
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

static ModuleInf *mod_system;
static ModuleInf *mod_user;


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
  int meta_arg[3];		/* max is for catch/3 */

#endif

  pl_module_tbl = Pl_Hash_Alloc_Table(START_MODULE_TBL_SIZE, sizeof(ModuleInf));
  mod_system = Pl_Create_Module(pl_atom_system);
  mod_user = Pl_Create_Module(pl_atom_user);

  pl_pred_tbl = mod_user->pred_tbl;

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

  meta_arg[0] = meta_arg[1] = meta_arg[2] = 0; /* default = 0 (integer), e.g. ','(0,0) */

  Pl_Create_Pred_Meta(pl_atom_system, ATOM_CHAR(','), 2, file, __LINE__, prop, NULL, meta_arg);

  Pl_Create_Pred_Meta(pl_atom_system, ATOM_CHAR(';'), 2, file, __LINE__, prop, NULL, meta_arg);

  Pl_Create_Pred_Meta(pl_atom_system, Pl_Create_Atom("->"), 2, file, __LINE__, prop, NULL, meta_arg);

  Pl_Create_Pred_Meta(pl_atom_system, Pl_Create_Atom("*->"), 2, file, __LINE__, prop, NULL, meta_arg);

  Pl_Create_Pred(pl_atom_system, ATOM_CHAR('!'), 0, file, __LINE__, prop, NULL);

  Pl_Create_Pred(pl_atom_system, Pl_Create_Atom("fail"), 0, file, __LINE__, prop, NULL);

  Pl_Create_Pred(pl_atom_system, pl_atom_true, 0, file, __LINE__, prop, NULL);

  Pl_Create_Pred_Meta(pl_atom_system, Pl_Create_Atom("call"), 1, file, __LINE__, prop, NULL, meta_arg);

  meta_arg[1] = META_PRED_ARG_QUESTION; /* for catch(0,?,0) */
  Pl_Create_Pred_Meta(pl_atom_system, Pl_Create_Atom("catch"), 3, file, __LINE__, prop, NULL, meta_arg);

  Pl_Create_Pred(pl_atom_system, Pl_Create_Atom("throw"), 1, file, __LINE__, prop, NULL);

#endif
}



/*-------------------------------------------------------------------------*
 * PL_CREATE_MODULE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
ModuleInf *
Pl_Create_Module(int module)
{
  ModuleInf module_info;
  ModuleInf *mod;


#ifdef DEBUG
  DBGPRINTF("Create module: %s\n", pl_atom_tbl[module].name);
#endif

  module_info.module = module;
  module_info.pred_tbl = NULL;

  Pl_Extend_Table_If_Needed(&pl_module_tbl);
  mod = (ModuleInf *) Pl_Hash_Insert(pl_module_tbl, (char *) &module_info, FALSE);
  if (mod->pred_tbl == NULL)
    mod->pred_tbl = Pl_Hash_Alloc_Table(START_PRED_TBL_SIZE, sizeof(PredInf));
  
  return mod;
}




/*-------------------------------------------------------------------------*
 * PL_LOOKUP_MODULE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
ModuleInf *
Pl_Lookup_Module(int module)
{

  if (module == pl_atom_user)
    return mod_user;

  if (module == pl_atom_system)
    return mod_system;

  return (ModuleInf *) Pl_Hash_Find(pl_module_tbl, module);
}




/*-------------------------------------------------------------------------*
 * PL_DELETE_MODULE                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Delete_Module(int module)
{
  Pl_Hash_Delete(pl_module_tbl, module);
}




/*-------------------------------------------------------------------------*
 * PL_CREATE_PRED                                                          *
 *                                                                         *
 * Called by compiled prolog code, by dynamic predicate support and by     *
 * byte-code support.                                                      *
 *-------------------------------------------------------------------------*/
PredInf * FC
Pl_Create_Pred(int module, int func, int arity, int pl_file, int pl_line, int prop,
	       PlLong *codep)
{
  ModuleInf *mod = Pl_Create_Module(module);
  PredInf pred_info;
  PredInf *pred;
  PlLong key = Functor_Arity(func, arity);


  if (prop & (MASK_PRED_BUILTIN_FD | MASK_PRED_CONTROL_CONSTRUCT))
    prop |= MASK_PRED_BUILTIN;	/* now an FD built-in or a CC is also a built-in */

#ifdef DEBUG
  DBGPRINTF("Create pred: %s:%s/%d  prop: %x\n",
	    pl_atom_tbl[module].name, pl_atom_tbl[func].name, arity, prop);
#endif

  pred_info.f_n = key;
  pred_info.mod = mod;
  pred_info.prop = prop;
  pred_info.pl_file = pl_file;
  pred_info.pl_line = pl_line;
  pred_info.codep = codep;
  pred_info.dyn = NULL;

  Pl_Extend_Table_If_Needed(&mod->pred_tbl);
  pred = (PredInf *) Pl_Hash_Insert(mod->pred_tbl, (char *) &pred_info, FALSE);

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
 * PL_CREATE_PRED_META                                                     *
 *                                                                         *
 * Called by compiled prolog code, by dynamic predicate support and by     *
 * byte-code support.                                                      *
 *-------------------------------------------------------------------------*/
PredInf * FC
Pl_Create_Pred_Meta(int module, int func, int arity, int pl_file, int pl_line, int prop,
		    PlLong *codep, int meta_arg[])
{
  PredInf *pred = Pl_Create_Pred(module, func, arity, pl_file, pl_file, 
				 prop | MASK_PRED_META_PRED, codep);
  MetaSpec meta_spec = 0;
  int i;

  for(i = 0; i < arity; i++)
    meta_spec |= (meta_arg[i] << (i * 4));

  pred->meta_spec = meta_spec;

  return pred;
}




/*-------------------------------------------------------------------------*
 * PL_LOOKUP_PRED                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PredInf * FC
Pl_Lookup_Pred(int module, int func, int arity)
{
  ModuleInf *mod;
  PredInf *pred = NULL;
  PlLong key = Functor_Arity(func, arity);

  if (module != pl_atom_system)
    {
      if (module != pl_atom_user)
	{
	  mod = Pl_Lookup_Module(module);

	  if (mod != NULL && (pred = (PredInf *) Pl_Hash_Find(mod->pred_tbl, key)) != NULL)
	    return pred;
	}

      if ((pred = (PredInf *) Pl_Hash_Find(mod_user->pred_tbl, key)) != NULL)
	return pred;
    }

  pred = (PredInf *) Pl_Hash_Find(mod_system->pred_tbl, key);
  return pred;
}




/*-------------------------------------------------------------------------*
 * PL_DELETE_PRED                                                          *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void FC
Pl_Delete_Pred(int module, int func, int arity)
{
  ModuleInf *mod = Pl_Lookup_Module(module);
  PlLong key = Functor_Arity(func, arity);

  if (mod)
    Pl_Hash_Delete(mod->pred_tbl, key);
}




/* TO BE REMOVED - COMPAT ONLY */

/*-------------------------------------------------------------------------*
 * PL_CREATE_PRED_COMPAT                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PredInf *
Pl_Create_Pred_Compat(int func, int arity, int pl_file, int pl_line, 
		      int prop, long *codep)
{
  return Pl_Create_Pred(pl_atom_user, func, arity, pl_file, pl_line, prop, codep);
}



/*-------------------------------------------------------------------------*
 * PL_LOOKUP_PRED_COMPAT                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
PredInf *
Pl_Lookup_Pred_Compat(int func, int arity)
{
  return Pl_Lookup_Pred(pl_atom_user, func, arity);
}




/*-------------------------------------------------------------------------*
 * PL_DELETE_PRED_COMPAT                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Delete_Pred_Compat(int func, int arity)
{
  return Pl_Delete_Pred(pl_atom_user, func, arity);
}
