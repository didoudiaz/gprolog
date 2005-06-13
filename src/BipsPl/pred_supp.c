/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : pred_supp.c                                                     *
 * Descr.: predicate management support                                    *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2005 Daniel Diaz                                     *
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
 * DETECT_IF_AUX_NAME                                                      *
 *                                                                         *
 * returns NULL if not an aux name or a pointer to / before the arity of   *
 * the father.                                                             *
 *-------------------------------------------------------------------------*/
char *
Detect_If_Aux_Name(int func)
{
  char *str = atom_tbl[func].name;
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
 * FATHER_PRED_OF_AUX                                                      *
 *                                                                         *
 * returns -1 if it is not an aux predicate name.                          *
 *-------------------------------------------------------------------------*/
int
Father_Pred_Of_Aux(int func, int *father_arity)
{
  char *p;
  int l;

  p = Detect_If_Aux_Name(func);
  if (p == NULL)
    return -1;

  l = p - atom_tbl[func].name;

  *father_arity = strtol(p + 1, NULL, 10);
  strcpy(glob_buff, atom_tbl[func].name + 1);	/* skip 1st $ */
  glob_buff[l - 1] = '\0';

  return Create_Allocate_Atom(glob_buff);
}




/*-------------------------------------------------------------------------*
 * PRED_WITHOUT_AUX                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Pred_Without_Aux(int func, int arity, int *arity1)
{
  int func1;

  func1 = Father_Pred_Of_Aux(func, arity1);
  if (func1 < 0)
    {
      *arity1 = arity;
      func1 = func;
    }

  return func1;
}




/*-------------------------------------------------------------------------*
 * MAKE_AUX_NAME                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
int
Make_Aux_Name(int func, int arity, int aux_nb)
{
  func = Pred_Without_Aux(func, arity, &arity);

  sprintf(glob_buff, "$%s/%d%s%d", atom_tbl[func].name, arity, AUX_STR,
	  aux_nb);
  return Create_Allocate_Atom(glob_buff);
}
