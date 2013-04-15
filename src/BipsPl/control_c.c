/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : control_c.c                                                     *
 * Descr.: control management - C part                                     *
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

#define BETWEEN_ALT                    X1_246265747765656E5F616C74

Prolog_Prototype(BETWEEN_ALT, 0);




/*-------------------------------------------------------------------------*
 * PL_HALT_IF_NO_TOP_LEVEL_1                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
WamCont
Pl_Halt_If_No_Top_Level_1(WamWord exit_code_word)
{
  PredInf *pred;
  int x;

  x = Pl_Rd_Integer_Check(exit_code_word);

  if (SYS_VAR_TOP_LEVEL == 0)	/* no top level running */
    Pl_Exit_With_Value(x);

  pred = Pl_Lookup_Pred(Pl_Create_Atom((x) ? "$top_level_abort" : "$top_level_stop"), 0);

  if (pred == NULL)		/* should not occur */
    Pl_Exit_With_Value(x);

  return (WamCont) (pred->codep);
}




/*-------------------------------------------------------------------------*
 * PL_HALT_1                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Halt_1(WamWord exit_code_word)
{
  Pl_Exit_With_Value(Pl_Rd_Integer_Check(exit_code_word));
}




/*-------------------------------------------------------------------------*
 * PL_BETWEEN_3                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Between_3(WamWord l_word, WamWord u_word, WamWord i_word)
{
  WamWord word, tag_mask;
  PlLong l, u, i;

  l = Pl_Rd_Integer_Check(l_word);
  u = Pl_Rd_Integer_Check(u_word);

  DEREF(i_word, word, tag_mask);
  if (tag_mask != TAG_REF_MASK)
    {
      i = Pl_Rd_Integer_Check(word);
      return i >= l && i <= u;
    }
  i_word = word;

  if (l > u)
    return FALSE;
				/* here i_word is a variable */
  if (l < u)			/* non deterministic case */
    {
      A(0) = l + 1;
      A(1) = u;
      A(2) = i_word;
      Pl_Create_Choice_Point((CodePtr) Prolog_Predicate(BETWEEN_ALT, 0), 3);
    }

  return Pl_Get_Integer(l, i_word); /* always TRUE */
}




/*-------------------------------------------------------------------------*
 * PL_BETWEEN_ALT_0                                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Between_Alt_0(void)
{
  PlLong l, u;
  WamWord i_word;

  Pl_Update_Choice_Point((CodePtr) Prolog_Predicate(BETWEEN_ALT, 0), 0);

  l = AB(B, 0);
  u = AB(B, 1);
  i_word = AB(B, 2);

  /* here i_word is a variable */
  if (l == u)
    Delete_Last_Choice_Point();
  else				/* non deterministic case */
    {
      AB(B, 0) = l + 1;
#if 0 /* the following data is unchanged */
      AB(B, 1) = u;
      AB(B, 2) = i_word;
#endif
    }

  Pl_Get_Integer(l, i_word);	/* always TRUE */
}
