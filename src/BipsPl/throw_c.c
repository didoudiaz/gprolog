/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : throw_c.c                                                       *
 * Descr.: exception management (throw) - C part                           *
 * Author: Daniel Diaz                                                     *
 *                                                                         *
 * Copyright (C) 1999-2010 Daniel Diaz                                     *
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

WamWord *pl_query_top_b;		/* overwritten by foreign_supp if present */
WamWord pl_query_exception;	/* overwritten by foreign_supp if present */




/*---------------------------------*
 * Function Prototypes             *
 *---------------------------------*/




/*-------------------------------------------------------------------------*
 * PL_THROW_2                                                              *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Pl_Throw_2(WamWord ball_word, WamWord b_word)
{
  WamWord word, tag_mask;
  WamWord *b;
  StmInf *pstm;

  DEREF(b_word, word, tag_mask);
  b = From_WamWord_To_B(word);

  if (b <= pl_query_top_b && pl_query_top_b != NULL)
    {
      Assign_B(pl_query_top_b);
      pl_query_exception = ball_word;
      Pl_Exit_With_Exception();
    }

  if (b == LSSA)
    {
      pstm = pl_stm_tbl[pl_stm_top_level_output];

      Pl_Stream_Printf(pstm, "\nsystem_error(cannot_catch_throw(");
      Pl_Write_Term(pstm, -1, MAX_PREC,
		 WRITE_NUMBER_VARS | WRITE_NAME_VARS | WRITE_QUOTED,
		 ball_word);
      Pl_Stream_Printf(pstm, "))\n");
      return;
    }

  Pl_Cut(b_word);
}
