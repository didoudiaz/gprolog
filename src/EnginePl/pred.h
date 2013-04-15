/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : pred.h                                                          *
 * Descr.: predicate table management - header file                        *
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



/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

					  /* if modified -> modif wam2ma.c */
#define MASK_PRED_NATIVE_CODE       1	  /* codep is set, except for control constructs */
#define MASK_PRED_DYNAMIC           2	  /* dynamic or static */
#define MASK_PRED_PUBLIC            4	  /* public or private */
#define MASK_PRED_BUILTIN           8	  /* built-in (procedure provided by the system) */
#define MASK_PRED_BUILTIN_FD        16	  /* FD built-in pred  (==> MASK_PRED_BUILTIN) */
#define MASK_PRED_CONTROL_CONSTRUCT 32	  /* control_construct (==> MASK_PRED_BUILTIN) */
#define MASK_PRED_MULTIFILE         64	  /* multifile or monofile */
#define MASK_PRED_EXPORTED          128	  /* exported by module not yet used - for future */





/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct			/* Predicate information          */
{				/* ------------------------------ */
  PlLong f_n;			/* key is <functor_atom,arity>    */
  int pl_file;			/* atom pl file of its definiton  */
  int pl_line;			/* pl file line of its definition */
  int prop;			/* predicate props (cf BipsPl)    */
  PlLong *codep;		/* compiled code                  */
  PlLong *dyn;			/* dynamic info (cf BipsPl)       */
}
PredInf;




/*---------------------------------*
 * Global Variables                *
 *---------------------------------*/

#ifdef PRED_FILE

char *pl_pred_tbl;

#else

extern char *pl_pred_tbl;

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Pl_Init_Pred(void);

PredInf * FC Pl_Create_Pred(int func, int arity, int pl_file, int pl_line,
			    int prop, PlLong *codep);

PredInf * FC Pl_Lookup_Pred(int func, int arity);

void FC Pl_Delete_Pred(int func, int arity);
