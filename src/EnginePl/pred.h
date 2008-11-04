/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : pred.h                                                          *
 * Descr.: predicate table management - header file                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2008 Daniel Diaz                                     *
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

/*---------------------------------*
 * Constants                       *
 *---------------------------------*/

/*---------------------------------*
 * Type Definitions                *
 *---------------------------------*/

typedef struct			/* Predicate information          */
{				/* ------------------------------ */
  long f_n;			/* key is <functor_atom,arity>    */
  int pl_file;			/* atom pl file of its definiton  */
  int pl_line;			/* pl file line of its definition */
  int prop;			/* predicate props (cf BipsPl)    */
  long *codep;			/* compiled code                  */
  long *dyn;			/* dynamic info (cf BipsPl)       */
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
		     int prop, long *codep);

PredInf * FC Pl_Lookup_Pred(int func, int arity);

void FC Pl_Delete_Pred(int func, int arity);
