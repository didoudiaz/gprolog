/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog engine                                                   *
 * File  : pred.h                                                          *
 * Descr.: predicate table management - header file                        *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2003 Daniel Diaz                                     *
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

char *pred_tbl;

#else

extern char *pred_tbl;

#endif




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/

void Init_Pred(void);

PredInf *Create_Pred(int func, int arity, int pl_file, int pl_line,
		     int prop, long *codep) FC;

PredInf *Lookup_Pred(int func, int arity) FC;

void Delete_Pred(int func, int arity) FC;
