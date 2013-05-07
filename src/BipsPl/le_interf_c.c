/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : le_interf_c.c                                                   *
 * Descr.: linedit interface management - C part                           *
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


#include <ctype.h>

#include "engine_pl.h"
#include "bips_pl.h"

#include "linedit.h"




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

#define FIND_LINEDIT_COMPLETION_ALT X1_2466696E645F6C696E656469745F636F6D706C6574696F6E5F616C74

Prolog_Prototype(FIND_LINEDIT_COMPLETION_ALT, 0);




/*-------------------------------------------------------------------------*
 * PL_GET_LINEDIT_PROMPT_1                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Get_Linedit_Prompt_1(WamWord prompt_word)
{
  return Pl_Un_String_Check(pl_le_prompt, prompt_word);
}




/*-------------------------------------------------------------------------*
 * PL_SET_LINEDIT_PROMPT_1                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Set_Linedit_Prompt_1(WamWord prompt_word)
{
  pl_le_prompt = pl_atom_tbl[Pl_Rd_Atom_Check(prompt_word)].name;
}




/*-------------------------------------------------------------------------*
 * PL_ADD_LINEDIT_COMPLETION_1                                             *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Add_Linedit_Completion_1(WamWord compl_word)
{
  int atom;
  AtomProp prop;
  char *p;

  atom = Pl_Rd_Atom_Check(compl_word);
  prop = pl_atom_tbl[atom].prop;

  if (prop.length == 0)
    return FALSE;

  if (prop.type != IDENTIFIER_ATOM)
    {
      for (p = pl_atom_tbl[atom].name; *p; p++)
	if (!isalnum(*p) && *p != '_')
	  return FALSE;
    }

  Pl_LE_Compl_Add_Word(pl_atom_tbl[atom].name, prop.length);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * PL_FIND_LINEDIT_COMPLETION_2                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Find_Linedit_Completion_2(WamWord prefix_word, WamWord compl_word)
{
  char *prefix = Pl_Rd_String_Check(prefix_word);
  int nb_match, max_lg, is_last;
  char *compl;

  Pl_Check_For_Un_Atom(compl_word);

  if (Pl_LE_Compl_Init_Match(prefix, &nb_match, &max_lg) == NULL)
    return FALSE;

  compl = Pl_LE_Compl_Find_Match(&is_last);

  if (!is_last)			/* non deterministic case */
    {
      A(0) = compl_word;
      Pl_Create_Choice_Point((CodePtr)
			  Prolog_Predicate(FIND_LINEDIT_COMPLETION_ALT, 0),
			  1);
    }

  return Pl_Get_Atom(Pl_Create_Atom(compl), compl_word);
}




/*-------------------------------------------------------------------------*
 * PL_FIND_LINEDIT_COMPLETION_ALT_0                                        *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Pl_Find_Linedit_Completion_Alt_0(void)
{
  WamWord compl_word;
  int is_last;
  char *compl;

  Pl_Update_Choice_Point((CodePtr)
		      Prolog_Predicate(FIND_LINEDIT_COMPLETION_ALT, 0), 0);

  compl_word = AB(B, 0);

  compl = Pl_LE_Compl_Find_Match(&is_last);

  if (is_last)
    Delete_Last_Choice_Point();

  return Pl_Get_Atom(Pl_Create_Atom(compl), compl_word);
}
