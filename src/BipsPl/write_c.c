/*-------------------------------------------------------------------------*
 * GNU Prolog                                                              *
 *                                                                         *
 * Part  : Prolog buit-in predicates                                       *
 * File  : write_c.c                                                       *
 * Descr.: term output (write/1 and friends) management - C part           *
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




/*-------------------------------------------------------------------------*
 * WRITE_TERM_2                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Write_Term_2(WamWord sora_word, WamWord term_word)
{
  int stm;
  StmInf *pstm;


  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_output : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);
  pstm = stm_tbl[stm];

  last_output_sora = sora_word;
  Check_Stream_Type(stm, TRUE, FALSE);

  Write_Term(pstm, SYS_VAR_WRITE_DEPTH, SYS_VAR_WRITE_PREC, 
	     SYS_VAR_OPTION_MASK, term_word);
}




/*-------------------------------------------------------------------------*
 * WRITE_TERM_1                                                            *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Write_Term_1(WamWord term_word)
{
  Write_Term_2(NOT_A_WAM_WORD, term_word);
}




/*-------------------------------------------------------------------------*
 * WRITE_1                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Write_1(WamWord term_word)
{
  SYS_VAR_OPTION_MASK = WRITE_NUMBER_VARS | WRITE_NAME_VARS;
  SYS_VAR_WRITE_DEPTH = -1;
  SYS_VAR_WRITE_PREC = MAX_PREC;

  Write_Term_2(NOT_A_WAM_WORD, term_word);
}




/*-------------------------------------------------------------------------*
 * WRITE_2                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Write_2(WamWord sora_word, WamWord term_word)
{
  SYS_VAR_OPTION_MASK = WRITE_NUMBER_VARS | WRITE_NAME_VARS;
  SYS_VAR_WRITE_DEPTH = -1;
  SYS_VAR_WRITE_PREC = MAX_PREC;

  Write_Term_2(sora_word, term_word);
}




/*-------------------------------------------------------------------------*
 * WRITEQ_1                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Writeq_1(WamWord term_word)
{
  SYS_VAR_OPTION_MASK = WRITE_NUMBER_VARS | WRITE_NAME_VARS | WRITE_QUOTED;
  SYS_VAR_WRITE_DEPTH = -1;
  SYS_VAR_WRITE_PREC = MAX_PREC;

  Write_Term_2(NOT_A_WAM_WORD, term_word);
}




/*-------------------------------------------------------------------------*
 * WRITEQ_2                                                                *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Writeq_2(WamWord sora_word, WamWord term_word)
{
  SYS_VAR_OPTION_MASK = WRITE_NUMBER_VARS | WRITE_NAME_VARS | WRITE_QUOTED;
  SYS_VAR_WRITE_DEPTH = -1;
  SYS_VAR_WRITE_PREC = MAX_PREC;

  Write_Term_2(sora_word, term_word);
}




/*-------------------------------------------------------------------------*
 * WRITE_CANONICAL_1                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Write_Canonical_1(WamWord term_word)
{
  SYS_VAR_OPTION_MASK = WRITE_IGNORE_OP | WRITE_QUOTED;
  SYS_VAR_WRITE_DEPTH = -1;
  SYS_VAR_WRITE_PREC = MAX_PREC;

  Write_Term_2(NOT_A_WAM_WORD, term_word);
}




/*-------------------------------------------------------------------------*
 * WRITE_CANONICAL_2                                                       *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Write_Canonical_2(WamWord sora_word, WamWord term_word)
{
  SYS_VAR_OPTION_MASK = WRITE_IGNORE_OP | WRITE_QUOTED;
  SYS_VAR_WRITE_DEPTH = -1;
  SYS_VAR_WRITE_PREC = MAX_PREC;

  Write_Term_2(sora_word, term_word);
}




/*-------------------------------------------------------------------------*
 * DISPLAY_1                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Display_1(WamWord term_word)
{
  SYS_VAR_OPTION_MASK = WRITE_IGNORE_OP;
  SYS_VAR_WRITE_DEPTH = -1;
  SYS_VAR_WRITE_PREC = MAX_PREC;

  Write_Term_2(NOT_A_WAM_WORD, term_word);
}




/*-------------------------------------------------------------------------*
 * DISPLAY_2                                                               *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Display_2(WamWord sora_word, WamWord term_word)
{
  SYS_VAR_OPTION_MASK = WRITE_IGNORE_OP;
  SYS_VAR_WRITE_DEPTH = -1;
  SYS_VAR_WRITE_PREC = MAX_PREC;

  Write_Term_2(sora_word, term_word);
}




/*-------------------------------------------------------------------------*
 * PRINT_1                                                                 *
 *                                                                         *
 * NB: the definition of the predicate print/1-2 is in the file print.pl   *
 * to avoid to link call/1 if print is not used.                           *
 *-------------------------------------------------------------------------*/
void
Print_1(WamWord term_word)
{
  SYS_VAR_OPTION_MASK =
    WRITE_NUMBER_VARS | WRITE_NAME_VARS | WRITE_PORTRAYED;
  SYS_VAR_WRITE_DEPTH = -1;
  SYS_VAR_WRITE_PREC = MAX_PREC;

  Write_Term_2(NOT_A_WAM_WORD, term_word);
}




/*-------------------------------------------------------------------------*
 * PRINT_2                                                                 *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Print_2(WamWord sora_word, WamWord term_word)
{
  SYS_VAR_OPTION_MASK =
    WRITE_NUMBER_VARS | WRITE_NAME_VARS | WRITE_PORTRAYED;
  SYS_VAR_WRITE_DEPTH = -1;
  SYS_VAR_WRITE_PREC = MAX_PREC;

  Write_Term_2(sora_word, term_word);
}




/*-------------------------------------------------------------------------*
 * NL_1                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Nl_1(WamWord sora_word)
{
  int stm;


  stm = (sora_word == NOT_A_WAM_WORD)
    ? stm_output : Get_Stream_Or_Alias(sora_word, STREAM_CHECK_OUTPUT);

  last_output_sora = sora_word;
  Check_Stream_Type(stm, TRUE, FALSE);

  Stream_Putc('\n', stm_tbl[stm]);
}




/*-------------------------------------------------------------------------*
 * NL_0                                                                    *
 *                                                                         *
 *-------------------------------------------------------------------------*/
void
Nl_0(void)
{
  Nl_1(NOT_A_WAM_WORD);
}
