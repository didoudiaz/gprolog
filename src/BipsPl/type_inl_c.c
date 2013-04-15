/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : type_inl_c.c                                                    *
 * Descr.: type testing (inline) management - C part                       *
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


#include "engine_pl.h"
#include "bips_pl.h"




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

	  /* Type tests */

#define Tag_Is_Var(t)              (t == TAG_REF_MASK)

#define Tag_Is_Nonvar(t)           (!Tag_Is_Var(t))

#define Tag_Is_Atom(t)             (t == TAG_ATM_MASK)

#define Tag_Is_Integer(t)          (t == TAG_INT_MASK)

#define Tag_Is_Float(t)            (t == TAG_FLT_MASK)

#define Tag_Is_Number(t)           (Tag_Is_Integer(t) || Tag_Is_Float(t))

#define Tag_Is_Atomic(t)           (Tag_Is_Atom(t) || Tag_Is_Number(t))

#define Tag_Is_Compound(t)         (t == TAG_STC_MASK || t == TAG_LST_MASK)

#define Tag_Is_Callable(t)         (Tag_Is_Atom(t) || Tag_Is_Compound(t))

#ifndef NO_USE_FD_SOLVER
#define Tag_Is_Fd_Var(t)           (t == TAG_FDV_MASK)
#else
#define Tag_Is_Fd_Var(t)           (FALSE)
#endif

#define Tag_Is_Non_Fd_Var(t)       (!Tag_Is_Fd_Var(t))

#define Tag_Is_Generic_Var(t)      (Tag_Is_Var(t) || Tag_Is_Fd_Var(t))

#define Tag_Is_Non_Generic_Var(t)  (!Tag_Is_Generic_Var(t))


#define Type_Test(test, x)			\
   WamWord word, tag_mask;  			\
   DEREF(x, word, tag_mask);			\
   return test(tag_mask)



Bool FC
Pl_Blt_Var(WamWord x)
{
  Type_Test(Tag_Is_Var, x);
}

Bool FC
Pl_Blt_Non_Var(WamWord x)
{
  Type_Test(Tag_Is_Nonvar, x);
}

Bool FC
Pl_Blt_Atom(WamWord x)
{
  Type_Test(Tag_Is_Atom, x);
}

Bool FC
Pl_Blt_Integer(WamWord x)
{
  Type_Test(Tag_Is_Integer, x);
}

Bool FC
Pl_Blt_Float(WamWord x)
{
  Type_Test(Tag_Is_Float, x);
}

Bool FC
Pl_Blt_Number(WamWord x)
{
  Type_Test(Tag_Is_Number, x);
}

Bool FC
Pl_Blt_Atomic(WamWord x)
{
  Type_Test(Tag_Is_Atomic, x);
}

Bool FC
Pl_Blt_Compound(WamWord x)
{
  Type_Test(Tag_Is_Compound, x);
}

Bool FC
Pl_Blt_Callable(WamWord x)
{
  Type_Test(Tag_Is_Callable, x);
}

Bool FC
Pl_Blt_Fd_Var(WamWord x)
{
  Type_Test(Tag_Is_Fd_Var, x);
}

Bool FC
Pl_Blt_Non_Fd_Var(WamWord x)
{
  Type_Test(Tag_Is_Non_Fd_Var, x);
}

Bool FC
Pl_Blt_Generic_Var(WamWord x)
{
  Type_Test(Tag_Is_Generic_Var, x);
}

Bool FC
Pl_Blt_Non_Generic_Var(WamWord x)
{
  Type_Test(Tag_Is_Non_Generic_Var, x);
}




/*-------------------------------------------------------------------------*
 * PL_BLT_GROUND                                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Blt_Ground(WamWord start_word)
{
  WamWord word, tag_mask;
  WamWord *adr;
  int arity;

 terminal_rec:
  DEREF(start_word, word, tag_mask);

  if (Tag_Is_Generic_Var(tag_mask))
    return FALSE;

  if (tag_mask == TAG_LST_MASK)
    {
      adr = UnTag_LST(word);
      if (!Pl_Blt_Ground(Car(adr)))
	return FALSE;

      start_word = Cdr(adr);
      goto terminal_rec;
    }

  if (tag_mask == TAG_STC_MASK)
    {
      adr = UnTag_LST(word);
      arity = Arity(adr);

      adr = &Arg(adr, 0);
      while(--arity > 0)
	{
	  if (!Pl_Blt_Ground(*adr++))
	    return FALSE;

	}

      start_word = *adr;
      goto terminal_rec;
    }

  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_BLT_LIST                                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Blt_List(WamWord start_word)
{
  WamWord word, tag_mask;

  for (;;)
    {
      DEREF(start_word, word, tag_mask);

      if (word == NIL_WORD)
	return TRUE;

      if (tag_mask != TAG_LST_MASK)
	return FALSE;

      start_word = Cdr(UnTag_LST(word));
    }
}




/*-------------------------------------------------------------------------*
 * PL_BLT_PARTIAL_LIST                                                     *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Blt_Partial_List(WamWord start_word)
{
  WamWord word, tag_mask;

  for (;;)
    {
      DEREF(start_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK)
	return TRUE;

      if (tag_mask != TAG_LST_MASK)
	return FALSE;

      start_word = Cdr(UnTag_LST(word));
    }
}




/*-------------------------------------------------------------------------*
 * PL_BLT_LIST_OR_PARTIAL_LIST                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool FC
Pl_Blt_List_Or_Partial_List(WamWord start_word)
{
  WamWord word, tag_mask;

  for (;;)
    {
      DEREF(start_word, word, tag_mask);

      if (tag_mask == TAG_REF_MASK || word == NIL_WORD)
	return TRUE;

      if (tag_mask != TAG_LST_MASK)
	return FALSE;

      start_word = Cdr(UnTag_LST(word));
    }
}
