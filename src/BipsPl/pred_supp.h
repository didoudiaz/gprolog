/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : pred_supp.h                                                     *
 * Descr.: predicate management support - header file                      *
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

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

					  /* if modified -> modif wam2ma.c */
#define MASK_PRED_NATIVE_CODE      1
#define MASK_PRED_DYNAMIC          2
#define MASK_PRED_PUBLIC           4
#define MASK_PRED_BUILTIN          8
#define MASK_PRED_BUILTIN_FD       16
#define MASK_PRED_MULTIFILE        32
#define MASK_PRED_EXPORTED         64 	  /* not yet used - for future */

#define MASK_PRED_ANY_BUILTIN      (MASK_PRED_BUILTIN | MASK_PRED_BUILTIN_FD)




/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

char *Pl_Detect_If_Aux_Name(int func);

int Pl_Father_Pred_Of_Aux(int func, int *father_arity);

int Pl_Pred_Without_Aux(int func, int arity, int *arity1);

int Pl_Make_Aux_Name(int func, int arity, int aux_nb);
