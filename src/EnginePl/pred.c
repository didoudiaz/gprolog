/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog engine                                                   */
/* File  : pred.c                                                          */
/* Descr.: predicate table management                                      */
/* Author: Daniel Diaz                                                     */
/*                                                                         */
/* Copyright (C) 1999,2000 Daniel Diaz                                     */
/*                                                                         */
/* GNU Prolog is free software; you can redistribute it and/or modify it   */
/* under the terms of the GNU General Public License as published by the   */
/* Free Software Foundation; either version 2, or any later version.       */
/*                                                                         */
/* GNU Prolog is distributed in the hope that it will be useful, but       */
/* WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        */
/* General Public License for more details.                                */
/*                                                                         */
/* You should have received a copy of the GNU General Public License along */
/* with this program; if not, write to the Free Software Foundation, Inc.  */
/* 59 Temple Place - Suite 330, Boston, MA 02111, USA.                     */
/*-------------------------------------------------------------------------*/
#include <stdlib.h>
#include <string.h>


#define PRED_FILE

#include "engine_pl.h"




/*---------------------------------*/
/* Constants                       */
/*---------------------------------*/

/*---------------------------------*/
/* Type Definitions                */
/*---------------------------------*/

/*---------------------------------*/
/* Global Variables                */
/*---------------------------------*/

/*---------------------------------*/
/* Function Prototypes             */
/*---------------------------------*/




/*-------------------------------------------------------------------------*/
/* INIT_PRED                                                               */
/*                                                                         */
/*-------------------------------------------------------------------------*/
void Init_Pred(void)

{
 pred_tbl=Hash_Alloc_Table(START_PRED_TBL_SIZE,sizeof(PredInf));
}




/*-------------------------------------------------------------------------*/
/* CREATE_PRED                                                             */
/*                                                                         */
/* Called by compiled prolog code, by dynamic predicate support and by     */
/* byte-code support.                                                      */
/*-------------------------------------------------------------------------*/
PredInf *Create_Pred(int func,int arity,int pl_file,int pl_line,int prop,
                     long *codep)

{
 PredInf  pred_info;
 PredInf *pred;
 long     key=Functor_Arity(func,arity);


#ifdef DEBUG
 DBGPRINTF("Create pred: %s/%d  prop: %x\n",atom_tbl[func].name,arity,prop);
#endif

 pred_info.f_n    =key;
 pred_info.prop   =prop;
 pred_info.pl_file=pl_file;
 pred_info.pl_line=pl_line;
 pred_info.codep  =codep;
 pred_info.dyn    =NULL;

 Extend_Table_If_Needed(&pred_tbl);
 pred=(PredInf *) Hash_Insert(pred_tbl,(char *) &pred_info,FALSE);

 return pred;
}




/*-------------------------------------------------------------------------*/
/* LOOKUP_PRED                                                             */
/*                                                                         */
/*-------------------------------------------------------------------------*/
PredInf *Lookup_Pred(int func,int arity)

{
 long key=Functor_Arity(func,arity);

 return (PredInf *) Hash_Find(pred_tbl,key);
}




/*-------------------------------------------------------------------------*/
/* DELETE_PRED                                                             */
/*                                                                         */
/*-------------------------------------------------------------------------*/
void Delete_Pred(int func,int arity)

{
 long key=Functor_Arity(func,arity);

 Hash_Delete(pred_tbl,key);
}
