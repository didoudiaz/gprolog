/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : le_interf_c.c                                                   *
 * Descr.: linedit interface management - C part                           *
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

#define FIND_LINEDIT_COMPLETION_ALT X2466696E645F6C696E656469745F636F6D706C6574696F6E5F616C74

Prolog_Prototype(FIND_LINEDIT_COMPLETION_ALT, 0);




/*-------------------------------------------------------------------------*
 * GET_LINEDIT_PROMPT_1                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Get_Linedit_Prompt_1(WamWord prompt_word)
{
  return Un_String_Check(le_prompt, prompt_word);
}




/*-------------------------------------------------------------------------*
 * SET_LINEDIT_PROMPT_1                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Set_Linedit_Prompt_1(WamWord prompt_word)
{
  le_prompt = atom_tbl[Rd_Atom_Check(prompt_word)].name;
}




/*-------------------------------------------------------------------------*
 * ADD_LINEDIT_COMPLETION_1                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Add_Linedit_Completion_1(WamWord compl_word)
{
  int atom;
  AtomProp prop;
  char *p;

  atom = Rd_Atom_Check(compl_word);
  prop = atom_tbl[atom].prop;

  if (prop.length == 0)
    return FALSE;

  if (prop.type != IDENTIFIER_ATOM)
    {
      for (p = atom_tbl[atom].name; *p; p++)
	if (!isalnum(*p) && *p != '_')
	  return FALSE;
    }

  LE_Compl_Add_Word(atom_tbl[atom].name, prop.length);
  return TRUE;
}




/*-------------------------------------------------------------------------*
 * FIND_LINEDIT_COMPLETION_2                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Find_Linedit_Completion_2(WamWord prefix_word, WamWord compl_word)
{
  char *prefix = Rd_String_Check(prefix_word);
  int nb_match, max_lg, is_last;
  char *compl;

  Check_For_Un_Atom(compl_word);

  if (LE_Compl_Init_Match(prefix, &nb_match, &max_lg) == NULL)
    return FALSE;

  compl = LE_Compl_Find_Match(&is_last);

  if (!is_last)			/* non deterministic case */
    {
      A(0) = compl_word;
      Create_Choice_Point((CodePtr)
			  Prolog_Predicate(FIND_LINEDIT_COMPLETION_ALT, 0),
			  1);
    }

  return Get_Atom(Create_Atom(compl), compl_word);
}




/*-------------------------------------------------------------------------*
 * FIND_LINEDIT_COMPLETION_ALT_0                                           *
 *                                                                         *
 *-------------------------------------------------------------------------*/
Bool
Find_Linedit_Completion_Alt_0(void)
{
  WamWord compl_word;
  int is_last;
  char *compl;

  Update_Choice_Point((CodePtr)
		      Prolog_Predicate(FIND_LINEDIT_COMPLETION_ALT, 0), 0);

  compl_word = AB(B, 0);

  compl = LE_Compl_Find_Match(&is_last);

  if (is_last)
    Delete_Last_Choice_Point();

  return Get_Atom(Create_Atom(compl), compl_word);
}
