/*-------------------------------------------------------------------------*/
/* GNU Prolog                                                              */
/*                                                                         */
/* Part  : Prolog buit-in predicates                                       */
/* File  : type_inl_c.c                                                    */
/* Descr.: type testing (inline) management - C part                       */
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

#include "engine_pl.h"
#include "bips_pl.h"




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




          /* Type tests */


#define Tag_Is_Var(t)              ((t)==REF)
#define Tag_Is_Nonvar(t)           (!Tag_Is_Var(t))
#define Tag_Is_Atom(t)             ((t)==ATM)
#define Tag_Is_Integer(t)          ((t)==INT)
#define Tag_Is_Float(t)            ((t)==FLT)
#define Tag_Is_Number(t)           (Tag_Is_Integer(t) || Tag_Is_Float(t))
#define Tag_Is_Atomic(t)           (Tag_Is_Atom(t) || Tag_Is_Number(t))
#define Tag_Is_Compound(t)         ((t)==STC || (t)==LST)
#define Tag_Is_Callable(t)         ((t)==ATM || (t)==STC || (t)==LST)

#define Tag_Is_Fd_Var(t)           ((t)==FDV)
#define Tag_Is_Non_Fd_Var(t)       (!Tag_Is_Fd_Var(t))
#define Tag_Is_Generic_Var(t)      (Tag_Is_Var(t) || Tag_Is_Fd_Var(t))
#define Tag_Is_Non_Generic_Var(t)  (!Tag_Is_Generic_Var(t))




#define Type_Test(test,x)                                                   \
    {                                                                       \
     WamWord word,tag,*adr;                                                 \
                                                                            \
     Deref(x,word,tag,adr)                                                  \
     return test(tag);                                                      \
    }



Bool Blt_Var            (WamWord x) { Type_Test(Tag_Is_Var,x)      }
Bool Blt_Non_Var        (WamWord x) { Type_Test(Tag_Is_Nonvar,x)   }
Bool Blt_Atom           (WamWord x) { Type_Test(Tag_Is_Atom,x)     }
Bool Blt_Integer        (WamWord x) { Type_Test(Tag_Is_Integer,x)  }
Bool Blt_Float          (WamWord x) { Type_Test(Tag_Is_Float,x)    }
Bool Blt_Number         (WamWord x) { Type_Test(Tag_Is_Number,x)   }
Bool Blt_Atomic         (WamWord x) { Type_Test(Tag_Is_Atomic,x)   }
Bool Blt_Compound       (WamWord x) { Type_Test(Tag_Is_Compound,x) }
Bool Blt_Callable       (WamWord x) { Type_Test(Tag_Is_Callable,x) }

Bool Blt_Fd_Var         (WamWord x) { Type_Test(Tag_Is_Fd_Var,x) }
Bool Blt_Non_Fd_Var     (WamWord x) { Type_Test(Tag_Is_Non_Fd_Var,x) }
Bool Blt_Generic_Var    (WamWord x) { Type_Test(Tag_Is_Generic_Var,x) }
Bool Blt_Non_Generic_Var(WamWord x) { Type_Test(Tag_Is_Non_Generic_Var,x) }




/*-------------------------------------------------------------------------*/
/* BLT_LIST                                                                */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Blt_List(WamWord start_word)

{
 WamWord word,tag,*adr;

 for(;;)
    {
     Deref(start_word,word,tag,adr)

     if (word==NIL_WORD)
         return TRUE;

     if (tag!=LST)
         return FALSE;

     adr=UnTag_LST(word);
     start_word=Cdr(adr);
    }
}




/*-------------------------------------------------------------------------*/
/* BLT_PARTIAL_LIST                                                        */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Blt_Partial_List(WamWord start_word)

{
 WamWord word,tag,*adr;

 for(;;)
    {
     Deref(start_word,word,tag,adr)

     if (tag==REF)
         return TRUE;

     if (tag!=LST)
         return FALSE;

     adr=UnTag_LST(word);
     start_word=Cdr(adr);
    }
}




/*-------------------------------------------------------------------------*/
/* BLT_LIST_OR_PARTIAL_LIST                                                */
/*                                                                         */
/*-------------------------------------------------------------------------*/
Bool Blt_List_Or_Partial_List(WamWord start_word)

{
 WamWord word,tag,*adr;

 for(;;)
    {
     Deref(start_word,word,tag,adr)

     if (tag==REF || word==NIL_WORD)
         return TRUE;

     if (tag!=LST)
         return FALSE;

     adr=UnTag_LST(word);
     start_word=Cdr(adr);
    }
}
