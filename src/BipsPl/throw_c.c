/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : throw_c.c                                                       *
 * Descr.: exception management (throw) - C part                           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2001 Daniel Diaz                                     *
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

WamWord *query_top_b;		/* overwritten by foreign_supp if present */
WamWord query_exception;	/* overwritten by foreign_supp if present */




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/




/*-------------------------------------------------------------------------*
 * THROW_2                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Throw_2(WamWord ball_word, WamWord b_word)
{
  WamWord word, tag_mask;
  WamWord *b;
  StmInf *pstm;

  DEREF(b_word, word, tag_mask);
  b = From_WamWord_To_B(word);

  if (b <= query_top_b && query_top_b != NULL)
    {
      Assign_B(query_top_b);
      query_exception = ball_word;
      Exit_With_Exception();
    }

  if (b == LSSA)
    {
      pstm = stm_tbl + stm_top_level_output;

      Stream_Printf(pstm, "\nsystem_error(cannot_catch_throw(");
      Write_Term(pstm, -1, MAX_PREC,
		 WRITE_NUMBER_VARS | WRITE_NAME_VARS | WRITE_QUOTED,
		 ball_word);
      Stream_Printf(pstm, "))\n");
      return;
    }

  Cut(b_word);
}
