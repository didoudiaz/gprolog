/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : control_c.c                                                     *
 * Descr.: control management - C part                                     *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2004 Daniel Diaz                                     *
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

#define FOR_ALT                    X24666F725F616C74

Prolog_Prototype(FOR_ALT, 0);




/*-------------------------------------------------------------------------*
 * HALT_IF_NO_TOP_LEVEL_1                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamCont
Halt_If_No_Top_Level_1(WamWord exit_code_word)
{
  PredInf *pred;
  int x;

  x = Rd_Integer_Check(exit_code_word);

  if (SYS_VAR_TOP_LEVEL == 0)	/* no top level running */
    Exit_With_Value(x);

  pred =
    Lookup_Pred(Create_Atom((x) ? "$top_level_abort" : "$top_level_stop"),
		0);

  if (pred == NULL)		/* should not occur */
    Exit_With_Value(x);

  return (WamCont) (pred->codep);
}




/*-------------------------------------------------------------------------*
 * HALT_1                                                                  *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Halt_1(WamWord exit_code_word)
{
  Exit_With_Value(Rd_Integer_Check(exit_code_word));
}




/*-------------------------------------------------------------------------*
 * FOR_3                                                                   *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
For_3(WamWord i_word, WamWord l_word, WamWord u_word)
{
  WamWord word, tag_mask;
  int i, l, u;

  l = Rd_Integer_Check(l_word);
  u = Rd_Integer_Check(u_word);

  DEREF(i_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    {
      i = Rd_Integer_Check(word);
      return i >= l && i <= u;
    }
  i_word = word;

  if (l > u)
    return FALSE;
				/* here i_word is a variable */
  if (l < u)			/* non deterministic case */
    {
      A(0) = i_word;
      A(1) = l + 1;
      A(2) = u;
      Create_Choice_Point((CodePtr) Prolog_Predicate(FOR_ALT, 0), 3);
    }

  return Get_Integer(l, i_word); /* always TRUE */
}




/*-------------------------------------------------------------------------*
 * FOR_ALT_0                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
For_Alt_0(void)
{
  WamWord i_word;
  int l, u;

  Update_Choice_Point((CodePtr) Prolog_Predicate(FOR_ALT, 0), 0);

  i_word = AB(B, 0);
  l = AB(B, 1);
  u = AB(B, 2);

  /* here i_word is a variable */
  if (l == u)
    Delete_Last_Choice_Point();
  else				/* non deterministic case */
    {
#if 0 /* the following data is unchanged */
      AB(B,0)=i_word;
#endif
      AB(B, 1) = l + 1;
#if 0 /* the following data is unchanged */
      AB(B,2)=u;
#endif
    }

  Get_Integer(l, i_word);	/* always TRUE */
}
