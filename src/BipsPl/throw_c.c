/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : throw_c.c                                                       *
 * Descr.: exception management (throw) - C part                           *
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
		    WRITE_NUMBER_VARS | WRITE_NAME_VARS | WRITE_QUOTED, NULL,
		    ball_word);
      Pl_Stream_Printf(pstm, "))\n");
      return;
    }

  Pl_Cut(b_word);
}
