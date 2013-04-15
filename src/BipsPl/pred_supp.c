/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : pred_supp.c                                                     *
 * Descr.: predicate management support                                    *
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
#include <ctype.h>

#include "engine_pl.h"
#include "bips_pl.h"




/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

#define AUX_STR                    "_$aux"



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
 * PL_DETECT_IF_AUX_NAME                                                   *
 *                                                                         *
 * returns NULL if not an aux name or a pointer to / before the arity of   *
 * the father.                                                             *
 *-------------------------------------------------------------------------*/
char *
Pl_Detect_If_Aux_Name(int func)
{
  char *str = pl_atom_tbl[func].name;
  char *p, *q;


  if (*str != '$' || (p = strstr(str, AUX_STR)) == NULL)
    return NULL;

  q = p + sizeof(AUX_STR) - 1;
  if (!isdigit(*q))
    return NULL;

  while (isdigit(*++q))
    ;

  if (*q != '\0')
    return NULL;

  while (isdigit(*--p) && p > str)
    ;

  if (*p != '/')
    return NULL;

  return p;
}




/*-------------------------------------------------------------------------*
 * PL_FATHER_PRED_OF_AUX                                                   *
 *                                                                         *
 * returns -1 if it is not an aux predicate name.                          *
 *-------------------------------------------------------------------------*/
int
Pl_Father_Pred_Of_Aux(int func, int *father_arity)
{
  char *p;
  int l;

  p = Pl_Detect_If_Aux_Name(func);
  if (p == NULL)
    return -1;

  l = p - pl_atom_tbl[func].name;

  *father_arity = strtol(p + 1, NULL, 10);
  strcpy(pl_glob_buff, pl_atom_tbl[func].name + 1);	/* skip 1st $ */
  pl_glob_buff[l - 1] = '\0';

  return Pl_Create_Allocate_Atom(pl_glob_buff);
}




/*-------------------------------------------------------------------------*
 * PL_PRED_WITHOUT_AUX                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Pred_Without_Aux(int func, int arity, int *arity1)
{
  int func1;

  func1 = Pl_Father_Pred_Of_Aux(func, arity1);
  if (func1 < 0)
    {
      *arity1 = arity;
      func1 = func;
    }

  return func1;
}




/*-------------------------------------------------------------------------*
 * PL_MAKE_AUX_NAME                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pl_Make_Aux_Name(int func, int arity, int aux_nb)
{
  func = Pl_Pred_Without_Aux(func, arity, &arity);

  sprintf(pl_glob_buff, "$%s/%d%s%d", pl_atom_tbl[func].name, arity, AUX_STR,
	  aux_nb);
  return Pl_Create_Allocate_Atom(pl_glob_buff);
}




/*-------------------------------------------------------------------------*
 * PL_CREATE_PRED_MULTIFILE                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Emit_BC_Execute_Wrapper(int func, int arity, PlLong *codep)
{
  Pl_BC_Start_Emit_0();
  Pl_BC_Emit_Inst_Execute_Native(func, arity, codep);
  Pl_BC_Stop_Emit_0();
}
